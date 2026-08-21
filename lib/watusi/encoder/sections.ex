defmodule Watusi.Encoder.Sections do
  @moduledoc false
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Instructions, as: InstrEncoder
  alias Watusi.Instructions
  alias Watusi.LEB128
  import Bitwise

  # Mapping of WAT keywords to their corresponding WASM sections
  @section_map %{
    "func" => :funcs,
    "table" => :tables,
    "memory" => :memories,
    "global" => :globals,
    "tag" => :tags,
    "elem" => :elems,
    "data" => :data,
    "type" => :types,
    "start" => :starts
  }

  @importable_kinds ["func", "table", "memory", "global", "tag"]
  defguardp is_importable(kind) when kind in @importable_kinds

  @func_metadata_kinds ["param", "result", "local", "export"]
  defguardp is_func_metadata(kind) when kind in @func_metadata_kinds

  @inline_elem_entry_kinds ["ref.func", "ref.null", "global.get"]
  defguardp is_inline_elem_entry_kind(kind) when kind in @inline_elem_entry_kinds

  @indirect_call_ops ["call_indirect", "return_call_indirect"]
  defguardp is_indirect_call(name) when name in @indirect_call_ops

  @control_flow_ops ["block", "loop", "if", "try", "try_table"]
  defguardp is_control_flow(name) when name in @control_flow_ops

  @metadata_kinds ["param", "result", "type"]
  defguardp is_metadata_kind(name) when name in @metadata_kinds

  @offset_ops ["i32.const", "i64.const", "global.get"]
  defguardp is_offset_op(name) when name in @offset_ops

  # Opcodes that can be used as reference types in table/elem declarations
  @reftypes [
    "funcref",
    "anyfunc",
    "externref",
    "func",
    "anyref",
    "eqref",
    "structref",
    "arrayref",
    "i31ref",
    "nullref",
    "nullexternref",
    "nullfuncref"
  ]

  def group_sections(body) do
    initial = %{
      imports: [],
      funcs: [],
      tables: [],
      memories: [],
      globals: [],
      tags: [],
      elems: [],
      data: [],
      types: [],
      recs: [],
      starts: []
    }

    # We group items by their section type to allow for parallel processing
    # and to ensure correct section ordering in the final binary.
    body
    |> Enum.reduce(initial, &group_item/2)
    |> Map.new(fn {k, v} -> {k, Enum.reverse(v)} end)
  end

  defp group_item([{:keyword, "import"} | _] = item, acc) do
    %{acc | imports: [item | acc.imports]}
  end

  defp group_item([{:keyword, "table"} | _] = item, acc) do
    case inline_import?(item) do
      true ->
        %{acc | imports: [item | acc.imports]}

      false ->
        {table_item, inline_elem} = split_inline_table_elem(item)

        case inline_elem do
          nil ->
            %{acc | tables: [table_item | acc.tables]}

          elem_item ->
            %{acc | tables: [table_item | acc.tables], elems: [elem_item | acc.elems]}
        end
    end
  end

  defp group_item([{:keyword, "memory"} | _] = item, acc) do
    case inline_import?(item) do
      true ->
        %{acc | imports: [item | acc.imports]}

      false ->
        {memory_item, inline_data} = split_inline_memory_data(item)

        case inline_data do
          nil ->
            %{acc | memories: [memory_item | acc.memories]}

          data_item ->
            %{acc | memories: [memory_item | acc.memories], data: [data_item | acc.data]}
        end
    end
  end

  defp group_item([{:keyword, kind} | _] = item, acc) when is_importable(kind) do
    case inline_import?(item) do
      true -> %{acc | imports: [item | acc.imports]}
      false -> Map.update!(acc, Map.fetch!(@section_map, kind), &[item | &1])
    end
  end

  defp group_item([{:keyword, "rec"} | members] = item, acc) do
    flat_types = Enum.map(members, fn member -> member end)

    %{
      acc
      | types: Enum.reverse(flat_types) ++ acc.types,
        recs: [item | acc.recs]
    }
  end

  defp group_item([{:keyword, "type"} | _] = item, acc) do
    %{acc | types: [item | acc.types], recs: [item | acc.recs]}
  end

  defp group_item([{:keyword, kind} | _] = item, acc) when is_map_key(@section_map, kind) do
    Map.update!(acc, Map.fetch!(@section_map, kind), &[item | &1])
  end

  defp group_item(_, acc), do: acc

  # Inline imports like (func (import "env" "foo")) need to be extracted
  # into the Import section (ID 2) rather than the Function section.
  defp inline_import?(node) do
    Enum.any?(node, fn
      [{:keyword, "import"} | _] -> true
      _ -> false
    end)
  end

  defp split_inline_table_elem([{:keyword, "table"} | rest] = item) do
    inline_elem = Enum.find(rest, &match?([{:keyword, "elem"} | _], &1))

    case inline_elem do
      nil ->
        {item, nil}

      [{:keyword, "elem"} | funcs] ->
        is_table_64 = Enum.any?(rest, &match?({:keyword, "i64"}, &1))

        table_id =
          case rest do
            [{:id, id} | _] -> {:id, id}
            _ -> nil
          end

        reftype_node =
          Enum.find(rest, fn
            {:keyword, k} when k in @reftypes -> true
            [{:keyword, "ref"} | _] -> true
            _ -> false
          end)

        elem_item = build_inline_elem_item(table_id, is_table_64, funcs, reftype_node)

        base_table = Enum.reject(rest, &match?([{:keyword, "elem"} | _], &1))
        cleaned_table = build_table_item_from_inline(base_table, funcs, rest)

        {cleaned_table, elem_item}
    end
  end

  defp build_inline_elem_item(table_id, is_64, funcs, reftype_node) do
    offset_const = if is_64, do: "i64.const", else: "i32.const"

    reftype_part = if reftype_node, do: [reftype_node], else: []
    offset_node = [{:keyword, "offset"}, [{:keyword, offset_const}, {:int, 0}]]

    # Tag inline table elements so the encoder knows they are always active
    # initializers for their defining table (and must use the expr form).
    case table_id do
      nil ->
        [
          {:keyword, "elem"},
          {:inline_elem, true},
          offset_node
          | funcs ++ reftype_part
        ]

      id ->
        [
          {:keyword, "elem"},
          {:inline_elem, true},
          [{:keyword, "table"}, id],
          offset_node
          | funcs ++ reftype_part
        ]
    end
  end

  defp build_table_item_from_inline(base_table, funcs, full_rest) do
    has_explicit_limits = Enum.any?(full_rest, &match?({:int, _}, &1))

    case has_explicit_limits do
      true ->
        [{:keyword, "table"} | base_table]

      false ->
        elem_count = Enum.count(funcs, &inline_elem_entry?/1)
        [{:keyword, "table"} | base_table] ++ [{:int, elem_count}, {:int, elem_count}]
    end
  end

  defp split_inline_memory_data([{:keyword, "memory"} | rest] = item) do
    inline_data = Enum.find(rest, &match?([{:keyword, "data"} | _], &1))

    case inline_data do
      nil ->
        {item, nil}

      [{:keyword, "data"} | data_parts] ->
        is_64 = Enum.any?(rest, &match?({:keyword, "i64"}, &1))

        memory_id =
          case rest do
            [{:id, id} | _] -> {:id, id}
            _ -> nil
          end

        data_strings = Enum.filter(data_parts, &match?({:string, _}, &1))
        data_item = build_inline_data_item(memory_id, is_64, data_strings)

        base_memory = Enum.reject(rest, &match?([{:keyword, "data"} | _], &1))
        memory_item = build_memory_item_from_inline(base_memory, data_strings, memory_id, rest)

        {memory_item, data_item}
    end
  end

  defp build_inline_data_item(memory_id, is_64, data_strings) do
    offset_instr = if is_64, do: "i64.const", else: "i32.const"

    case memory_id do
      nil ->
        [
          {:keyword, "data"},
          [{:keyword, "offset"}, [{:keyword, offset_instr}, {:int, 0}]] | data_strings
        ]

      id ->
        [
          {:keyword, "data"},
          [{:keyword, "memory"}, id],
          [{:keyword, "offset"}, [{:keyword, offset_instr}, {:int, 0}]] | data_strings
        ]
    end
  end

  defp build_memory_item_from_inline(base_memory, data_strings, memory_id, full_rest) do
    has_explicit_limits = Enum.any?(full_rest, &match?({:int, _}, &1))

    case has_explicit_limits do
      true ->
        [{:keyword, "memory"} | base_memory]

      false ->
        total_size = Enum.reduce(data_strings, 0, fn {:string, s}, acc -> acc + byte_size(s) end)
        pages = div(total_size + 65_535, 65_536)

        case {data_strings, memory_id} do
          {[], nil} ->
            [{:keyword, "memory"}, {:int, 0}, {:int, 0} | base_memory]

          {[], id} ->
            [
              {:keyword, "memory"},
              id,
              {:int, 0},
              {:int, 0} | Enum.reject(base_memory, &(&1 == id))
            ]

          {_, nil} ->
            [{:keyword, "memory"}, {:int, pages}, {:int, pages} | base_memory]

          {_, id} ->
            [
              {:keyword, "memory"},
              id,
              {:int, pages},
              {:int, pages} | Enum.reject(base_memory, &(&1 == id))
            ]
        end
    end
  end

  defp inline_elem_entry?({:id, _}), do: true
  defp inline_elem_entry?({:int, _}), do: true

  defp inline_elem_entry?([{:keyword, k} | _]) when is_inline_elem_entry_kind(k),
    do: true

  defp inline_elem_entry?([{:keyword, "item"} | _]), do: true
  defp inline_elem_entry?(_), do: false

  def collect_import_signatures(imports, types) do
    Enum.flat_map(imports, fn
      [{:keyword, "import"}, _, _, [{:keyword, "func"} | rest]] ->
        untagged_import_sig([{:keyword, "func"} | rest], types)

      item ->
        case normalize_import(item) do
          {_, _, "func", rest} -> untagged_import_sig([{:keyword, "func"} | rest], types)
          {_, _, "tag", rest} -> untagged_import_sig([{:keyword, "func"} | rest], types)
          _ -> []
        end
    end)
  end

  defp untagged_import_sig(func, types) do
    if has_explicit_type?(func), do: [], else: [extract_signature(func, types)]
  end

  def extract_signature([{:keyword, "func"} | rest] = func, types) do
    # Function signatures can be declared inline or via a type index/ID
    case Enum.find(rest, &match?([{:keyword, "type"}, _], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        type_item =
          Enum.find(types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
            raise("Type not found: $#{id}")

        extract_raw_signature(type_item)

      [{:keyword, "type"}, {:int, i}] ->
        # Type index - will be resolved later, return a placeholder
        {:type_ref, i}

      _ ->
        extract_raw_signature(func)
    end
  end

  def extract_signature_index([{:keyword, "func"} | rest] = func, signatures, types, recs) do
    case Enum.find(rest, &match?([{:keyword, "type"}, _], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        Enum.find_index(types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
          raise("Type not found: $#{id}")

      [{:keyword, "type"}, {:int, i}] ->
        i

      _ ->
        resolve_untagged_func_index(func, signatures, types, recs)
    end
  end

  defp resolve_untagged_func_index(func, signatures, types, recs) do
    sig = extract_raw_signature(func)
    n_declared = length(types)

    case top_level_index(recs, sig) do
      nil ->
        (Enum.find_index(Enum.drop(signatures, n_declared), &(&1 == sig)) || 0) + n_declared

      top ->
        top
    end
  end

  defp top_level_index(recs, target) do
    {sig_map, _} =
      Enum.reduce(recs, {%{}, -1}, fn item, {sig_map, idx} ->
        case item do
          [{:keyword, "type"} | _] ->
            next = idx + 1
            {record_top_level_signature(sig_map, extract_raw_signature(item), next), next}

          [{:keyword, "rec"} | members] ->
            {sig_map, idx + length(members)}
        end
      end)

    Map.get(sig_map, target)
  end

  defp record_top_level_signature(sig_map, {p, r}, next) when is_list(p) and is_list(r),
    do: Map.put_new(sig_map, {p, r}, next)

  defp record_top_level_signature(sig_map, _other, _next), do: sig_map

  def prepare_signatures(sections) do
    # signatures are unique types in the module
    type_sigs = Enum.map(sections.types, &extract_raw_signature/1)

    tag_sigs =
      Enum.flat_map(sections.tags, fn [{:keyword, "tag"} | rest] ->
        tag = [{:keyword, "func"} | rest]
        if has_explicit_type?(tag), do: [], else: [extract_raw_signature(tag)]
      end)

    func_and_block_sigs =
      Enum.flat_map(sections.funcs, fn func ->
        func_sig =
          if has_explicit_type?(func),
            do: [],
            else: [extract_signature(func, sections.types)]

        func_sig ++ scan_for_signatures(func)
      end)

    existing_sigs = top_level_existing_sigs(sections.recs)

    other_sigs =
      [
        collect_import_signatures(sections.imports, sections.types),
        tag_sigs,
        func_and_block_sigs
      ]
      |> List.flatten()
      |> Enum.reject(&match?({:type_ref, _}, &1))
      |> Enum.reduce({[], existing_sigs}, fn sig, {acc, seen} ->
        case MapSet.member?(seen, sig) do
          true -> {acc, seen}
          false -> {[sig | acc], MapSet.put(seen, sig)}
        end
      end)
      |> elem(0)
      |> Enum.reverse()

    type_sigs ++ other_sigs
  end

  defp has_explicit_type?([{:keyword, _kind} | rest]) do
    Enum.any?(rest, &match?([{:keyword, "type"} | _], &1))
  end

  defp has_explicit_type?(_), do: false

  defp top_level_existing_sigs(recs) do
    Enum.flat_map(recs, fn
      [{:keyword, "rec"} | _] ->
        []

      type_ent ->
        case extract_raw_signature(type_ent) do
          {p, r} when is_list(p) and is_list(r) -> [extract_raw_signature(type_ent)]
          _ -> []
        end
    end)
    |> MapSet.new()
  end

  defp scan_for_signatures(term) when is_list(term) do
    do_scan_for_signatures(term)
  end

  defp scan_for_signatures(_), do: []

  defp do_scan_for_signatures([]), do: []

  defp do_scan_for_signatures([{:keyword, name} | rest])
       when is_indirect_call(name) do
    {args, remaining} = InstrEncoder.collect_args(rest, [])
    has_type_use = Enum.any?(args, &match?([{:keyword, "type"}, _], &1))

    current =
      case has_type_use do
        true ->
          []

        false ->
          [extract_param_result_metadata(args)]
      end

    current ++ Enum.flat_map(args, &scan_for_signatures/1) ++ do_scan_for_signatures(remaining)
  end

  defp do_scan_for_signatures([{:keyword, name} | rest])
       when is_control_flow(name) do
    {args, remaining} = InstrEncoder.collect_args(rest, [])
    {params, results} = extract_block_metadata(args)

    current =
      case params != [] or length(results) > 1 do
        true -> [{params, results}]
        false -> []
      end

    current ++ Enum.flat_map(args, &scan_for_signatures/1) ++ do_scan_for_signatures(remaining)
  end

  defp do_scan_for_signatures([head | tail]) do
    scan_for_signatures(head) ++ do_scan_for_signatures(tail)
  end

  defp extract_block_metadata(args) do
    args
    |> Enum.take_while(fn
      {:id, _} -> true
      [{:keyword, k} | _] when is_metadata_kind(k) -> true
      _ -> false
    end)
    |> extract_param_result_metadata()
  end

  defp extract_param_result_metadata(args) do
    Enum.reduce(args, {[], []}, fn
      [{:keyword, "param"} | ts], {params_acc, results_acc} ->
        params =
          Enum.reduce(ts, [], fn
            {:id, _}, acc -> acc
            {:keyword, t}, acc -> acc ++ [t]
            t, acc when is_list(t) -> acc ++ [t]
            _, acc -> acc
          end)

        {params_acc ++ params, results_acc}

      [{:keyword, "result"} | ts], {params_acc, results_acc} ->
        results =
          Enum.reduce(ts, [], fn
            {:keyword, t}, acc -> acc ++ [t]
            t, acc when is_list(t) -> acc ++ [t]
            _, acc -> acc
          end)

        {params_acc, results_acc ++ results}

      _, acc ->
        acc
    end)
  end

  def extract_raw_signature([{:keyword, "type"} | rest]) do
    # Skip optional ID
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    case rest do
      [[{:keyword, "func"} | inner] | _] -> extract_raw_signature([{:keyword, "func"} | inner])
      [[{:keyword, "struct"} | inner] | _] -> {:struct, inner}
      [[{:keyword, "array"} | inner] | _] -> {:array, inner}
      [[{:keyword, "sub"} | _] = sub | _] -> extract_raw_signature(unwrap_sub(sub))
      _ -> {[], []}
    end
  end

  def extract_raw_signature([{:keyword, "func"} | rest]) do
    # For function nodes, we extract 'param' and 'result' metadata
    metadata =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end
      |> Enum.take_while(fn
        [{:keyword, k} | _] when is_func_metadata(k) -> true
        _ -> false
      end)

    params =
      metadata
      |> Enum.flat_map(fn
        [{:keyword, "param"} | ts] -> ts
        _ -> []
      end)
      |> Enum.reject(&match?({:id, _}, &1))
      |> Enum.map(&normalize_type/1)

    results =
      metadata
      |> Enum.flat_map(fn
        [{:keyword, "result"} | ts] -> ts
        _ -> []
      end)
      |> Enum.map(&normalize_type/1)

    {params, results}
  end

  defp unwrap_sub([{:keyword, "sub"} | rest]) do
    composite = composite_from_sub(rest)
    [{:keyword, "type"}, composite]
  end

  defp composite_from_sub(rest) do
    case Enum.find(rest, &is_list/1) do
      nil -> [{:keyword, "func"}]
      composite when is_list(composite) -> composite
    end
  end

  defp normalize_type({:keyword, t}), do: t
  defp normalize_type([{:keyword, "ref"}, _] = t), do: t
  defp normalize_type([{:keyword, "ref"}, {:keyword, "null"}, _] = t), do: t
  defp normalize_type(other), do: other

  def encode_signature({:struct, fields}, ctx) do
    # GC Struct type (0x5F) followed by vector of field types
    entries = Enum.flat_map(fields, &field_entries/1)
    [0x5F, Common.encode_vector(entries, &encode_field_entry(&1, ctx))]
  end

  def encode_signature({:array, field}, ctx) do
    # GC Array type (0x5E) followed by a single field type
    [0x5E, encode_field(field, ctx)]
  end

  def encode_signature({params, results}, ctx) do
    # Standard Function type (0x60)
    [
      0x60,
      Common.encode_vector(params, &encode_valtype(Instructions.valtype(&1), ctx)),
      Common.encode_vector(results, &encode_valtype(Instructions.valtype(&1), ctx))
    ]
  end

  # The Type section is encoded as a vector of RecGroups. Explicit `(rec ...)`
  # groups are wrapped in the rec marker (0x4E) and count as a single vector
  # entry even though they contribute multiple type indices.
  def encode_type_section(signatures, recs, ctx) do
    declared_groups = Enum.map(recs, &encode_declared_group(&1, ctx))
    declared_count = Enum.reduce(recs, 0, fn group, acc -> acc + member_count(group) end)
    _ = declared_count

    remaining = Enum.drop(signatures, declared_count)
    remaining_iodata = Enum.map(remaining, &encode_signature(&1, ctx))

    entries = declared_groups ++ remaining_iodata
    entry_count = length(recs) + length(remaining)

    Common.encode_section(1, [Common.encode_u32(entry_count), entries])
  end

  defp member_count([{:keyword, "rec"} | members]), do: length(members)
  defp member_count(_), do: 1

  defp encode_declared_group([{:keyword, "rec"} | members], ctx) do
    [0x4E, Common.encode_u32(length(members)), for(m <- members, do: encode_subtype(m, ctx))]
  end

  defp encode_declared_group(member, ctx), do: encode_subtype(member, ctx)

  defp encode_subtype([{:keyword, "type"} | rest], ctx) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    defn = List.first(rest)

    case defn do
      [{:keyword, "sub"} | sub_rest] -> encode_sub(sub_rest, ctx)
      [{:keyword, "final"}, {:keyword, "sub"} | sub_rest] -> encode_sub(sub_rest, ctx, 0x4F)
      _ -> encode_composite(defn, ctx)
    end
  end

  defp encode_sub(sub_rest, ctx), do: encode_sub(sub_rest, ctx, 0x50)

  defp encode_sub(sub_rest, ctx, banner) do
    {final?, sub_rest} =
      case sub_rest do
        [{:keyword, "final"} | tail] -> {true, tail}
        other -> {false, other}
      end

    {supers, composite} = split_supers(sub_rest, [])

    cond do
      final? and supers == [] ->
        encode_composite(composite, ctx)

      final? ->
        super_bytes = Enum.map(supers, &encode_super(&1, ctx))
        [0x4F, Common.encode_u32(length(supers)), super_bytes, encode_composite(composite, ctx)]

      true ->
        super_bytes = Enum.map(supers, &encode_super(&1, ctx))
        [banner, Common.encode_u32(length(supers)), super_bytes, encode_composite(composite, ctx)]
    end
  end

  defp split_supers([composite | _], acc) when is_list(composite) and composite != [],
    do: {Enum.reverse(acc), composite}

  defp split_supers([composite | []], acc), do: {Enum.reverse(acc), composite}
  defp split_supers([s | rest], acc), do: split_supers(rest, [s | acc])

  defp encode_super({:id, id}, ctx) do
    case Enum.find_index(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) do
      nil -> Common.encode_u32(0)
      idx -> Common.encode_u32(idx)
    end
  end

  defp encode_super({:int, i}, _ctx), do: Common.encode_u32(i)

  defp encode_composite(composite, ctx) do
    sig = extract_raw_signature([{:keyword, "type"}, composite])
    encode_signature(sig, ctx)
  end

  defp encode_field(field, ctx) do
    # A field can be (field i32) or (field (mut i32))
    {type, mut} =
      case field do
        [{:keyword, "field"} | rest] -> extract_field_type(rest)
        _ -> extract_field_type(field)
      end

    [encode_valtype(Instructions.valtype(type), ctx), mut]
  end

  # A WAT struct field like `(field i32 (ref $t))` declares multiple storage
  # entries, each becoming its own encoded field.
  defp field_entries([{:keyword, "field"} | types]) do
    types
    |> Enum.reject(&match?({:id, _}, &1))
    |> Enum.map(&token_to_field/1)
  end

  defp field_entries(field) when is_list(field) do
    field
    |> Enum.reject(&match?({:id, _}, &1))
    |> Enum.map(&token_to_field/1)
  end

  defp field_entries(field), do: [token_to_field(field)]

  defp token_to_field([{:keyword, "mut"}, type]), do: {type, 0x01}
  defp token_to_field(list) when is_list(list), do: {list, 0x00}
  defp token_to_field({:keyword, type}), do: {type, 0x00}

  defp encode_field_entry({type, mut}, ctx) do
    [encode_valtype(Instructions.valtype(type), ctx), mut]
  end

  defp extract_field_type([[{:keyword, "mut"}, type] | _]), do: {type, 0x01}
  defp extract_field_type([type | _]) when is_list(type), do: {type, 0x00}
  defp extract_field_type([{:keyword, type} | _]), do: {type, 0x00}
  defp extract_field_type({:keyword, type}), do: {type, 0x00}

  def encode_valtype({:ref, node}, ctx), do: resolve_heap_type(node, ctx)
  def encode_valtype(type, _ctx) when is_integer(type), do: [type]
  def encode_valtype(type, _ctx) when is_binary(type), do: [Instructions.valtype(type)]

  defp resolve_heap_type([{:keyword, "ref"}, {:keyword, "null"}, arg], ctx) do
    # Nullable ref with abstract heap types use the direct valtype encoding
    case arg do
      {:keyword, k} ->
        [Instructions.valtype(abstract_heap_valtype(k))]

      _ ->
        [0x63, resolve_heap_type_arg(arg, ctx, true)]
    end
  end

  defp resolve_heap_type([{:keyword, "ref"}, arg], ctx) do
    [0x64, resolve_heap_type_arg(arg, ctx, false)]
  end

  defp abstract_heap_valtype("func"), do: "funcref"
  defp abstract_heap_valtype("extern"), do: "externref"
  defp abstract_heap_valtype("any"), do: "anyref"
  defp abstract_heap_valtype("eq"), do: "eqref"
  defp abstract_heap_valtype("struct"), do: "structref"
  defp abstract_heap_valtype("array"), do: "arrayref"
  defp abstract_heap_valtype("i31"), do: "i31ref"
  defp abstract_heap_valtype("exn"), do: "exnref"
  defp abstract_heap_valtype("none"), do: "nullref"
  defp abstract_heap_valtype("nofunc"), do: "nullfuncref"
  defp abstract_heap_valtype("noextern"), do: "nullexternref"
  defp abstract_heap_valtype("noexn"), do: "nullexnref"

  defp resolve_heap_type_arg(arg, ctx, nullable) do
    case arg do
      {:id, id} -> resolve_heap_type_id(id, ctx, nullable)
      {:int, i} -> LEB128.encode_signed(i)
      {:keyword, k} -> LEB128.encode_signed(heap_type_opcode(k))
    end
  end

  # Encode a bare heaptype (cast-reftype context, e.g. ref.test/ref.cast/
  # br_on_cast immediates). Unlike a full valtype, there is no 0x63/0x64
  # prefix; abstract heap types collapse to their single byte and concrete
  # types encode as the raw type index.
  def encode_heaptype({:id, id}, ctx), do: resolve_heap_type_id(id, ctx, false)
  def encode_heaptype({:int, i}, _ctx), do: LEB128.encode_signed(i)

  def encode_heaptype({:keyword, k}, _ctx) when k in @reftypes,
    do: [Instructions.valtype(k)]

  def encode_heaptype({:keyword, k}, _ctx), do: [Instructions.valtype(abstract_heap_valtype(k))]

  defp heap_type_opcode("func"), do: -0x10
  defp heap_type_opcode("extern"), do: -0x11
  defp heap_type_opcode("any"), do: -0x12
  defp heap_type_opcode("eq"), do: -0x13
  defp heap_type_opcode("i31"), do: -0x14
  defp heap_type_opcode("struct"), do: -0x15
  defp heap_type_opcode("array"), do: -0x16
  defp heap_type_opcode("exn"), do: -0x17
  defp heap_type_opcode("none"), do: -0x18
  defp heap_type_opcode("noextern"), do: -0x19
  defp heap_type_opcode("nofunc"), do: -0x1A
  defp heap_type_opcode("noexn"), do: -0x1B

  defp resolve_heap_type_id(id, ctx, nullable) do
    case Enum.find_index(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) do
      nil -> pack_symbol(id, nullable)
      idx -> LEB128.encode_signed(idx)
    end
  end

  defp pack_symbol(id, nullable) do
    # Add $ prefix if not present (lexer strips it)
    id =
      case String.starts_with?(id, "$") do
        true -> id
        false -> "$" <> id
      end

    tag = String.length(id) <<< 1

    tag =
      case nullable do
        true -> tag ||| 1
        false -> tag
      end

    chars = String.to_charlist(id) |> Enum.take(3)

    val =
      Enum.reduce(Enum.with_index(chars), tag, fn {char, i}, acc ->
        acc ||| char <<< (8 * (i + 1))
      end)

    LEB128.encode_signed(val)
  end

  def encode_import_section([], _signatures, _types, _recs), do: []

  def encode_import_section(imports, signatures, types, recs) do
    ctx = %{signatures: signatures, types: types, recs: recs}

    [
      Common.encode_u32(length(imports))
      | Enum.map(imports, &encode_import(&1, signatures, types, recs, ctx))
    ]
  end

  def encode_import(item, signatures, types, recs \\ nil) do
    ctx = %{signatures: signatures, types: types, recs: recs}
    encode_import(item, signatures, types, recs, ctx)
  end

  def encode_import(item, signatures, types, recs, ctx) do
    {mod, name, kind, rest} = normalize_import(item)

    [
      Common.encode_string(mod),
      Common.encode_string(name) | do_encode_import(kind, rest, signatures, types, recs, ctx)
    ]
  end

  defp do_encode_import("func", rest, signatures, types, recs, _ctx) do
    type_idx = extract_signature_index([{:keyword, "func"} | rest], signatures, types, recs)
    [0x00, Common.encode_u32(type_idx)]
  end

  defp do_encode_import("table", rest, _signatures, _types, _recs, ctx) do
    type_node =
      Enum.find(rest, fn
        {:keyword, k} when k in @reftypes -> true
        [{:keyword, "ref"} | _] -> true
        _ -> false
      end) || "funcref"

    [
      0x01,
      encode_valtype(Instructions.valtype(type_node), ctx),
      encode_limits(rest)
    ]
  end

  defp do_encode_import("memory", rest, _signatures, _types, _recs, _ctx) do
    [0x02, encode_limits(rest)]
  end

  defp do_encode_import("global", rest, _signatures, _types, _recs, ctx) do
    type_desc =
      case rest do
        [{:id, _id}, desc] -> desc
        [desc] -> desc
      end

    {type, mut} = extract_global_type(type_desc)

    type_bytes =
      case type do
        {:ref, [{:keyword, "ref"}, {:keyword, "null"}, {:keyword, _}] = ref} ->
          encode_valtype({:ref, ref}, ctx)

        {:ref, [{:keyword, "ref"}, {:keyword, _}] = ref} ->
          encode_valtype({:ref, ref}, ctx)

        {:ref, _} ->
          encode_valtype(type, ctx)

        _ ->
          encode_valtype(Instructions.valtype(type), ctx)
      end

    [0x03, type_bytes, mut]
  end

  defp do_encode_import("tag", rest, signatures, types, recs, _ctx) do
    type_idx = extract_signature_index([{:keyword, "func"} | rest], signatures, types, recs)
    [0x04, 0x00, Common.encode_u32(type_idx)]
  end

  def normalize_import([
        {:keyword, "import"},
        {:string, mod},
        {:string, name},
        [{:keyword, kind} | rest]
      ]) do
    {mod, name, kind, rest}
  end

  def normalize_import([{:keyword, kind} | rest]) do
    import_node = Enum.find(rest, &match?([{:keyword, "import"} | _], &1))
    [{:keyword, "import"}, {:string, mod}, {:string, name}] = import_node

    other_rest =
      Enum.reject(rest, fn
        [{:keyword, "import"} | _] -> true
        _ -> false
      end)

    {mod, name, kind, other_rest}
  end

  def encode_table([{:keyword, "table"} | rest], ctx) do
    # Remove name and inline exports from the definition list before processing
    rest =
      Enum.reject(rest, fn
        {:id, _} -> true
        [{:keyword, "export"}, _] -> true
        _ -> false
      end)

    # Extract an inline initializer expression if present. It appears as the last list
    # node that is an instruction (e.g. [ref.func ...], [global.get ...]); this must be
    # distinguished from the reftype ([ref ...]) which is not an instruction.
    {rest, init_expr} = extract_table_init_expr(rest)

    # Split limit integers (and the i64 marker) from the reftype descriptor. They
    # may appear before or after the reftype depending on the WAT form.
    {limit_nodes, reftype_candidates} =
      Enum.split_with(rest, fn
        {:int, _} -> true
        {:keyword, "i64"} -> true
        _ -> false
      end)

    reftype_node =
      case reftype_candidates do
        [] -> "funcref"
        [rt | _] -> rt
      end

    reftype_bytes = encode_valtype(Instructions.valtype(reftype_node), ctx)
    limits_bytes = encode_limits(limit_nodes)

    case init_expr do
      nil ->
        [reftype_bytes, limits_bytes]

      ie ->
        init_instrs =
          ie
          |> InstrEncoder.collect_instructions(ctx)
          |> Enum.map(&InstrEncoder.encode_instruction(&1, ctx))

        [0x40, 0x00, reftype_bytes, limits_bytes, init_instrs, 0x0B]
    end
  end

  # Splits an inline table definition into the trailing initializer expression (if any)
  # and the remaining nodes. The initializer is the last list node that is an
  # instruction (e.g. [ref.func ...], [global.get ...]); a leading [ref ...] node is
  # a reftype descriptor, not an instruction, and is left in place.
  defp extract_table_init_expr(rest) do
    case Enum.reverse(rest) do
      [ie | tail] when is_list(ie) ->
        case ie do
          [{:keyword, "ref"} | _] -> {rest, nil}
          [{:keyword, _} | _] -> {Enum.reverse(tail), ie}
          _ -> {rest, nil}
        end

      _ ->
        {rest, nil}
    end
  end

  def encode_memory([{:keyword, "memory"} | rest]) do
    rest =
      Enum.reject(rest, fn
        {:id, _} -> true
        [{:keyword, "export"}, _] -> true
        [{:keyword, "data"} | _] -> true
        _ -> false
      end)

    encode_limits(rest)
  end

  def encode_limits(tokens) do
    # Support for Shared Memory and Memory64 extensions
    is_shared = Enum.any?(tokens, &match?({:keyword, "shared"}, &1))
    is_64 = Enum.any?(tokens, &match?({:keyword, "i64"}, &1))

    tokens =
      Enum.filter(tokens, fn
        {:int, _} -> true
        _ -> false
      end)

    base_flags =
      case is_64 do
        true -> 0x04
        false -> 0x00
      end

    case {tokens, is_shared} do
      {[], false} ->
        [base_flags, Common.encode_u32(0)]

      {[{:int, min}], true} ->
        [base_flags ||| 0x03, Common.encode_u32(min), Common.encode_u32(min)]

      {[{:int, min}], false} ->
        [base_flags, Common.encode_u32(min)]

      {[{:int, min}, {:int, max}], true} ->
        [base_flags ||| 0x03, Common.encode_u32(min), Common.encode_u32(max)]

      {[{:int, min}, {:int, max}], false} ->
        [base_flags ||| 0x01, Common.encode_u32(min), Common.encode_u32(max)]

      _ ->
        raise "Invalid limits: #{inspect(tokens)}"
    end
  end

  def encode_global([{:keyword, "global"} | rest], ctx) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    # Remove inline exports from the definition list before processing type
    rest =
      Enum.reject(rest, fn
        [{:keyword, "export"}, _] -> true
        _ -> false
      end)

    {type_desc, rest} = List.pop_at(rest, 0)
    {type, mut} = extract_global_type(type_desc)
    instructions = InstrEncoder.collect_instructions(rest, ctx)

    type_bytes =
      case type do
        {:ref, _} -> encode_valtype(type, ctx)
        _ -> encode_valtype(Instructions.valtype(type), ctx)
      end

    [
      type_bytes,
      mut,
      Enum.map(instructions, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B
    ]
  end

  defp extract_global_type({:keyword, t}), do: {t, 0x00}
  defp extract_global_type([{:keyword, "mut"}, {:keyword, t}]), do: {t, 0x01}
  # Handle reference types in globals: (ref $t), (ref null <heaptype>) or (mut (ref ...))
  defp extract_global_type([{:keyword, "ref"}, _] = t), do: {{:ref, t}, 0x00}
  defp extract_global_type([{:keyword, "ref"}, {:keyword, "null"}, _] = t), do: {{:ref, t}, 0x00}

  defp extract_global_type([{:keyword, "mut"}, [{:keyword, "ref"}, _] = t]),
    do: {{:ref, t}, 0x01}

  defp extract_global_type([{:keyword, "mut"}, [{:keyword, "ref"}, {:keyword, "null"}, _] = t]),
    do: {{:ref, t}, 0x01}

  def encode_tag([{:keyword, "tag"} | rest], signatures, _types) do
    # Tags (for Exception Handling) identify a signature for their payload
    sig = extract_raw_signature([{:keyword, "func"} | rest])

    type_idx =
      Enum.find_index(signatures, &(&1 == sig)) ||
        raise("Type not found for tag: #{inspect(sig)}")

    [0x00, Common.encode_u32(type_idx)]
  end

  def encode_elem([{:keyword, "elem"} | rest], ctx) do
    # Elements (Table initializers) can be active, passive, or declarative
    rest = strip_elem_id(rest)
    inline_elem? = Enum.any?(rest, &match?({:inline_elem, true}, &1))
    {explicit_table?, table_idx, rest} = resolve_elem_table_idx(rest, ctx)

    offset_node = find_elem_offset(rest)

    # Inline table elements are always active initializers for the table they
    # are attached to, even though the parser does not wrap them in an offset.
    is_passive = is_nil(offset_node) and not inline_elem?
    is_declarative = Enum.any?(rest, &match?({:keyword, "declare"}, &1)) and not inline_elem?

    reftype_node = find_elem_reftype_node(rest)
    reftype = reftype_node || "funcref"

    expr_nodes =
      rest
      |> extract_elem_expr_nodes()
      |> Enum.reject(&(&1 == offset_node))

    {bare_indices, indices, has_expr_payload} =
      resolve_elem_payload(rest, offset_node, expr_nodes, reftype, ctx)

    {use_expr_form, explicit_index?} =
      elem_encoding_plan(
        inline_elem?,
        is_passive,
        is_declarative,
        explicit_table?,
        has_expr_payload,
        reftype_node
      )

    # The element type byte is written for passive, declared and explicit-index
    # segments only. The implicit-table funcref forms (flags 0 and 4) write no
    # type byte; flags 4 carries no reftype at all, matching wasm-tools.
    write_type? = is_passive or is_declarative or explicit_index?

    type_byte = elem_type_byte(write_type?, use_expr_form, reftype, ctx)

    # In the expr form, bare function indices are wrapped as `ref.func` expressions.
    # In the legacy funcidx form, every item -- including a source `ref.func N` --
    # is stored as a bare function index (no opcode).
    encoded_exprs =
      elem_expr_nodes(use_expr_form, expr_nodes, bare_indices)
      |> encode_elem_exprs(ctx)

    flags = elem_flags(is_passive, is_declarative, explicit_index?, use_expr_form)

    encode_elem_segment(flags, type_byte, encoded_exprs, indices, table_idx, offset_node, ctx)
  end

  defp strip_elem_id([{:id, _id} | tail]), do: tail
  defp strip_elem_id(other), do: other

  defp find_elem_offset(rest) do
    Enum.find(rest, fn
      [{:keyword, "offset"} | _] -> true
      [{:keyword, name} | _] when is_offset_op(name) -> true
      _ -> false
    end)
  end

  defp find_elem_reftype_node(rest) do
    Enum.find(rest, fn
      {:keyword, k} when k in @reftypes -> true
      [{:keyword, "ref"} | _] -> true
      _ -> false
    end)
  end

  # An inline table element can use the legacy funcidx form when it declares
  # the abstract funcref type (or none) and carries only bare function indices.
  defp elem_uses_funcidx?(nil), do: true
  defp elem_uses_funcidx?({:keyword, k}) when k in ["funcref", "anyfunc"], do: true
  defp elem_uses_funcidx?([{:keyword, "ref"}, {:keyword, "null"}, {:keyword, "func"}]), do: true
  defp elem_uses_funcidx?(_), do: false

  # Decide the two flags that shape the encoding: whether items are stored as
  # expression payloads (expr form, flags 4/5/6/7) and whether the explicit
  # table-index bit (0x02) is set. The funcidx form (flags 0/1/2/3) is used only
  # when the source does not force an expression payload: non-inline segments
  # with no reftype written (bare `func` keyword or omitted), and inline table
  # elements that declare the abstract funcref type and carry only bare
  # function indices. Every other combination (an explicit reftype such as
  # `funcref`/`externref`/`(ref ...)`, or expression elements) uses the expr
  # form. wasm-tools sets the explicit table-index bit whenever the source
  # names the segment's table, including inline table elements and table zero,
  # and for active expression segments whose reftype is not the abstract
  # funcref (the MVP implicit-table encoding only supports funcref). A bare
  # numeric table reference (`(elem 0 ...)`) is also explicit; only funcref
  # segments with no table clause at all are implicit.
  defp elem_encoding_plan(
         inline_elem?,
         is_passive,
         is_declarative,
         explicit_table?,
         has_expr_payload,
         reftype_node
       ) do
    use_expr_form =
      if inline_elem? do
        not elem_uses_funcidx?(reftype_node) or has_expr_payload
      else
        not legacy_func_reftype?(reftype_node)
      end

    explicit_index? =
      elem_explicit_index?(
        is_passive,
        is_declarative,
        explicit_table?,
        inline_elem?,
        use_expr_form,
        reftype_node
      )

    {use_expr_form, explicit_index?}
  end

  defp elem_explicit_index?(
         true,
         _is_declarative,
         _explicit_table?,
         _inline?,
         _use_expr,
         _reftype
       ),
       do: false

  defp elem_explicit_index?(_is_passive, true, _explicit_table?, _inline?, _use_expr, _reftype),
    do: false

  defp elem_explicit_index?(
         _is_passive,
         _is_declarative,
         explicit_table?,
         inline?,
         use_expr,
         reftype
       ) do
    explicit_table? or inline? or (use_expr and not funcref_reftype?(reftype))
  end

  defp elem_type_byte(false, _use_expr, _reftype, _ctx), do: []
  defp elem_type_byte(true, true, reftype, ctx), do: encode_elem_reftype(reftype, ctx)
  defp elem_type_byte(true, false, _reftype, _ctx), do: [0x00]

  defp elem_expr_nodes(true, expr_nodes, bare_indices) do
    expr_nodes ++
      Enum.map(bare_indices, fn idx -> [{:keyword, "ref.func"}, {:int, idx}] end)
  end

  defp elem_expr_nodes(false, _expr_nodes, _bare_indices), do: []

  defp encode_elem_exprs(elem_expr_nodes, ctx) do
    Common.encode_vector(elem_expr_nodes, fn expr_node ->
      expr_instrs =
        [expr_node]
        |> InstrEncoder.collect_instructions(ctx)
        |> Enum.map(&InstrEncoder.encode_instruction(&1, ctx))

      [expr_instrs, 0x0B]
    end)
  end

  # Replicates wabt's ElemSegment::GetFlags: passive=1, declared=3,
  # explicit-index=2 (active only), use-elem-exprs=4.
  defp elem_flags(_is_passive, true, _explicit, use_expr),
    do: if(use_expr, do: 0x07, else: 0x03)

  defp elem_flags(true, false, _explicit, use_expr),
    do: if(use_expr, do: 0x05, else: 0x01)

  defp elem_flags(false, false, explicit, use_expr) do
    base = if(explicit, do: 0x02, else: 0x00)
    base + if(use_expr, do: 0x04, else: 0x00)
  end

  # Whether the element type collapses to the legacy funcidx form for a
  # non-inline segment. Only the bare `func` keyword or an omitted reftype do;
  # any written reftype (including `funcref` and `(ref func)`) forces the expr
  # form, matching wasm-tools.
  defp legacy_func_reftype?(nil), do: true
  defp legacy_func_reftype?({:keyword, "func"}), do: true
  defp legacy_func_reftype?(_), do: false

  # Whether the segment's reftype is the abstract funcref family, which is the
  # only type the implicit-table active forms (flags 0 and 4) can carry.
  defp funcref_reftype?(nil), do: true
  defp funcref_reftype?({:keyword, k}) when k in ["funcref", "anyfunc"], do: true
  defp funcref_reftype?([{:keyword, "ref"}, {:keyword, "null"}, {:keyword, "func"}]), do: true
  defp funcref_reftype?(_), do: false

  # The reftype byte written in the expr form. The abstract `funcref`/`externref`
  # types and their nullable `(ref null ...)` spellings collapse to the single
  # shorthand byte; a non-null `(ref ...)` reference is written as its full
  # valtype encoding.
  defp encode_elem_reftype([{:keyword, "ref"}, {:keyword, "func"}], _ctx), do: [0x64, 0x70]
  defp encode_elem_reftype([{:keyword, "ref"}, {:keyword, "extern"}], _ctx), do: [0x64, 0x6F]

  defp encode_elem_reftype([{:keyword, "ref"}, {:keyword, "null"}, {:keyword, "func"}], _ctx),
    do: [0x70]

  defp encode_elem_reftype([{:keyword, "ref"}, {:keyword, "null"}, {:keyword, "extern"}], _ctx),
    do: [0x6F]

  defp encode_elem_reftype({:keyword, k}, _ctx) when k in ["funcref", "anyfunc", "func"],
    do: [0x70]

  defp encode_elem_reftype({:keyword, "externref"}, _ctx), do: [0x6F]
  defp encode_elem_reftype(reftype, ctx), do: encode_valtype(Instructions.valtype(reftype), ctx)

  defp encode_elem_segment(
         flags,
         type_byte,
         encoded_exprs,
         _indices,
         _table_idx,
         _offset_node,
         _ctx
       )
       when flags in [0x05, 0x07] do
    [flags, type_byte, encoded_exprs]
  end

  defp encode_elem_segment(
         flags,
         type_byte,
         _encoded_exprs,
         indices,
         _table_idx,
         _offset_node,
         _ctx
       )
       when flags in [0x01, 0x03] do
    [flags, type_byte, Common.encode_vector(indices, &Common.encode_u32/1)]
  end

  defp encode_elem_segment(
         0x00,
         _type_byte,
         _encoded_exprs,
         indices,
         _table_idx,
         offset_node,
         ctx
       ) do
    offset_expr = extract_offset_expr(offset_node)
    offset_instrs = InstrEncoder.collect_instructions(offset_expr, ctx)

    [
      0x00,
      Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B,
      Common.encode_vector(indices, &Common.encode_u32/1)
    ]
  end

  defp encode_elem_segment(0x04, type_byte, encoded_exprs, _indices, _table_idx, offset_node, ctx) do
    # Active table-0 expr form (flags 4): offset + reftype byte + expr list, no table index.
    offset_expr = extract_offset_expr(offset_node)
    offset_instrs = InstrEncoder.collect_instructions(offset_expr, ctx)

    [
      0x04,
      Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B,
      type_byte,
      encoded_exprs
    ]
  end

  defp encode_elem_segment(0x02, type_byte, _encoded_exprs, indices, table_idx, offset_node, ctx) do
    # Active explicit-table legacy form (flag 2): table index + reftype byte + funcidx list.
    offset_expr = extract_offset_expr(offset_node)
    offset_instrs = InstrEncoder.collect_instructions(offset_expr, ctx)

    [
      0x02,
      Common.encode_u32(table_idx),
      Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B,
      type_byte,
      Common.encode_vector(indices, &Common.encode_u32/1)
    ]
  end

  defp encode_elem_segment(0x06, type_byte, encoded_exprs, _indices, table_idx, offset_node, ctx) do
    # Active explicit-table expr form (flag 6): table index + reftype byte + expr list.
    offset_expr = extract_offset_expr(offset_node)
    offset_instrs = InstrEncoder.collect_instructions(offset_expr, ctx)

    [
      0x06,
      Common.encode_u32(table_idx),
      Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B,
      type_byte,
      encoded_exprs
    ]
  end

  defp extract_offset_expr(offset_node) do
    case offset_node do
      [{:keyword, "offset"} | expr] -> expr
      other -> [other]
    end
  end

  defp resolve_elem_table_idx(rest, ctx) do
    case rest do
      [[{:keyword, "table"}, {:id, id}] | tail] ->
        {true, InstrEncoder.resolve_index(id, ctx.tables, ctx.imports, "table"), tail}

      [[{:keyword, "table"}, {:int, i}] | tail] ->
        {true, i, tail}

      [{:keyword, "table"}, {:id, id} | tail] ->
        {true, InstrEncoder.resolve_index(id, ctx.tables, ctx.imports, "table"), tail}

      [{:keyword, "table"}, {:int, i} | tail] ->
        {true, i, tail}

      # Inline table elements are tagged with a `{:inline_elem, true}` marker,
      # so the `[table, id]` pair sits one position deeper in the list. The
      # segment's table is always explicit.
      [{:inline_elem, true}, [{:keyword, "table"}, {:id, id}] | tail] ->
        {true, InstrEncoder.resolve_index(id, ctx.tables, ctx.imports, "table"), tail}

      [{:inline_elem, true}, [{:keyword, "table"}, {:int, i}] | tail] ->
        {true, i, tail}

      # A bare numeric table index (`(elem 0 ...)`) is an explicit reference.
      [{:int, i} | tail] ->
        {true, i, tail}

      other ->
        {false, 0, other}
    end
  end

  defp resolve_elem_payload(rest, offset_node, expr_nodes, _reftype, ctx) do
    expr_ref_func_indices =
      Enum.flat_map(expr_nodes, fn
        [{:keyword, "ref.func"}, {:id, id}] ->
          [InstrEncoder.resolve_index(id, ctx.funcs, ctx.imports, "func")]

        [{:keyword, "ref.func"}, {:int, i}] ->
          [i]

        _ ->
          []
      end)

    bare_indices =
      rest
      |> Enum.reject(&(&1 == offset_node))
      |> collect_elem_func_indices(ctx)

    indices = bare_indices ++ expr_ref_func_indices

    {bare_indices, indices, expr_nodes != []}
  end

  # Walks the flat element item list collecting bare function references. A
  # top-level index/id is a function reference; the text form `func <id|int>...`
  # marks the following id/int tokens (until the next keyword or paren) as
  # function references.
  defp collect_elem_func_indices(tokens, ctx) do
    tokens
    |> Enum.reduce({[], false}, fn
      {:keyword, "func"}, {acc, _} ->
        {acc, true}

      {:id, _} = id, {acc, true} ->
        {acc ++ [InstrEncoder.resolve_index(id, ctx.funcs, ctx.imports, "func")], true}

      {:int, i}, {acc, true} ->
        {acc ++ [i], true}

      {:id, _} = id, {acc, false} ->
        {acc ++ [InstrEncoder.resolve_index(id, ctx.funcs, ctx.imports, "func")], false}

      {:int, i}, {acc, false} ->
        {acc ++ [i], false}

      _, {acc, _} ->
        {acc, false}
    end)
    |> elem(0)
  end

  def encode_data([{:keyword, "data"} | rest], ctx) do
    # Data segments (Memory initializers) can be active or passive
    {memidx, rest} = resolve_data_mem_idx(rest, ctx)
    offset_expr_items = resolve_data_offset_items(rest)

    string =
      rest
      |> Enum.filter(&match?({:string, _}, &1))
      |> Enum.map_join("", fn {:string, s} -> s end)

    case offset_expr_items do
      [] ->
        # flag 0x01: passive segment
        [0x01, Common.encode_string(string)]

      items ->
        offset_expr =
          items
          |> Enum.flat_map(fn
            [{:keyword, "offset"} | expr] -> expr
            other -> [other]
          end)
          |> InstrEncoder.collect_instructions(ctx)
          |> Enum.map(&InstrEncoder.encode_instruction(&1, ctx))

        case memidx do
          0 ->
            [0x00, offset_expr, 0x0B, Common.encode_string(string)]

          _ ->
            [0x02, Common.encode_u32(memidx), offset_expr, 0x0B, Common.encode_string(string)]
        end
    end
  end

  defp resolve_data_mem_idx(rest, ctx) do
    case Enum.find(rest, fn
           [{:keyword, "memory"}, _] -> true
           {:int, _} -> true
           _ -> false
         end) do
      [{:keyword, "memory"}, {:id, id}] = mem_node ->
        {InstrEncoder.resolve_index(id, ctx.memories, ctx.imports, "memory"),
         Enum.reject(rest, &(&1 == mem_node))}

      [{:keyword, "memory"}, {:int, i}] = mem_node ->
        {i, Enum.reject(rest, &(&1 == mem_node))}

      {:int, i} = mem_node ->
        {i, Enum.reject(rest, &(&1 == mem_node))}

      _ ->
        {0, rest}
    end
  end

  defp resolve_data_offset_items(rest) do
    case Enum.find(rest, &match?([{:keyword, "offset"} | _], &1)) do
      nil ->
        Enum.reject(rest, fn
          {:string, _} -> true
          {:id, _} -> true
          [{:keyword, "memory"} | _] -> true
          _ -> false
        end)

      offset_item ->
        [offset_item]
    end
  end

  defp extract_elem_expr_nodes(rest) do
    Enum.flat_map(rest, fn
      [{:keyword, k} | _] = node when is_inline_elem_entry_kind(k) ->
        [node]

      [{:keyword, "item"} | item_rest] ->
        case normalize_item_expr(item_rest) do
          nil -> []
          node -> [node]
        end

      [{:keyword, "ref"} | _] ->
        []

      [{:keyword, "offset"} | _] ->
        []

      # Generic folded expression element, e.g. (ref.i31 (i32.const 9)).
      [{:keyword, _} | _] = node ->
        [node]

      _ ->
        []
    end)
  end

  defp normalize_item_expr([{:keyword, k} | _] = node)
       when is_inline_elem_entry_kind(k),
       do: node

  defp normalize_item_expr([[{:keyword, k} | _] = node | _])
       when is_inline_elem_entry_kind(k),
       do: node

  # Generic wrapped folded expression item, e.g. (item (ref.i31 (i32.const 9)))
  defp normalize_item_expr([[{:keyword, _} | _] = node | _]), do: node

  defp normalize_item_expr(_), do: nil

  def encode_func_body([{:keyword, "func"} | rest] = func, ctx) do
    # Function bodies consist of local declarations followed by instructions
    local_map = InstrEncoder.build_local_map(func, ctx.types, ctx.sigs)

    locals =
      Enum.flat_map(rest, fn
        [{:keyword, "local"} | ts] ->
          Enum.reject(ts, &match?({:id, _}, &1))
          |> Enum.flat_map(fn
            {:keyword, t} -> [t]
            other when is_list(other) -> [other]
            _ -> []
          end)

        _ ->
          []
      end)

    grouped_locals = group_locals(locals)

    encoded_locals =
      Common.encode_vector(grouped_locals, fn {count, t} ->
        [Common.encode_u32(count), encode_valtype(Instructions.valtype(t), ctx)]
      end)

    instructions = InstrEncoder.collect_instructions(rest, ctx)
    func_ctx = %{ctx | local_map: local_map}

    # Semantic Validation (Work in progress)
    # sig = extract_signature(func, ctx.types)
    # Watusi.Validator.validate_function(instructions, sig, func_ctx)

    encoded_instrs = Enum.map(instructions, &InstrEncoder.encode_instruction(&1, func_ctx))

    body = [encoded_locals, encoded_instrs, 0x0B]
    [Common.encode_u32(IO.iodata_length(body)), body]
  end

  # Local variables are compressed by grouping consecutive items of the same type
  defp group_locals([]), do: []

  defp group_locals([first | rest]) do
    do_group_locals(rest, first, 1, [])
  end

  defp do_group_locals([], current_type, count, acc) do
    Enum.reverse([{count, current_type} | acc])
  end

  defp do_group_locals([type | rest], type, count, acc) do
    do_group_locals(rest, type, count + 1, acc)
  end

  defp do_group_locals([type | rest], current_type, count, acc) do
    do_group_locals(rest, type, 1, [{count, current_type} | acc])
  end

  # The Custom (name) section allows debuggers to show symbolic names for indices
  def encode_name_section(module_id, sections, counts, signatures) do
    func_names = collect_func_names(sections.imports, sections.funcs, counts.func)
    local_names = collect_local_names(sections.funcs, counts.func, sections.types, signatures)

    if module_id == nil and func_names == [] and local_names == [] do
      []
    else
      payload = [
        Common.encode_string("name"),
        encode_module_name_subsection(module_id),
        encode_func_name_subsection(func_names),
        encode_local_name_subsection(local_names)
      ]

      Common.encode_section(0, payload)
    end
  end

  defp encode_module_name_subsection(nil), do: []
  defp encode_module_name_subsection(id), do: encode_name_subsection(0, Common.encode_string(id))

  defp encode_func_name_subsection([]), do: []

  defp encode_func_name_subsection(names),
    do: encode_name_subsection(1, Common.encode_vector(names, &encode_name_assoc/1))

  defp encode_local_name_subsection([]), do: []

  defp encode_local_name_subsection(names),
    do: encode_name_subsection(2, Common.encode_vector(names, &encode_indirect_name_assoc/1))

  defp encode_name_subsection(id, payload),
    do: [id, Common.encode_u32(IO.iodata_length(payload)), payload]

  defp encode_name_assoc({idx, name}), do: [Common.encode_u32(idx), Common.encode_string(name)]

  defp encode_indirect_name_assoc({func_idx, map}),
    do: [Common.encode_u32(func_idx), Common.encode_vector(map, &encode_name_assoc/1)]

  defp collect_func_names(imports, funcs, func_import_count) do
    {import_entries, _} = Enum.map_reduce(imports, 0, &advance_func_import/2)
    import_names = Enum.reject(import_entries, &is_nil/1)

    local_names =
      Enum.with_index(funcs) |> Enum.flat_map(&extract_local_func_name(&1, func_import_count))

    import_names ++ local_names
  end

  defp advance_func_import(item, func_idx) do
    case normalize_import(item) do
      {_, _, "func", rest} -> named_func_import(Enum.find(rest, &match?({:id, _}, &1)), func_idx)
      _ -> {nil, func_idx}
    end
  end

  defp named_func_import({:id, id}, func_idx), do: {{func_idx, id}, func_idx + 1}
  defp named_func_import(nil, func_idx), do: {nil, func_idx + 1}

  defp extract_local_func_name({[{:keyword, "func"}, {:id, id} | _], idx}, offset),
    do: [{offset + idx, id}]

  defp extract_local_func_name(_, _), do: []

  defp collect_local_names(funcs, import_func_count, types, signatures) do
    Enum.with_index(funcs)
    |> Enum.flat_map(fn {func, idx} ->
      extract_func_local_names(func, import_func_count + idx, types, signatures)
    end)
  end

  defp extract_func_local_names(func, func_idx, types, signatures) do
    map = InstrEncoder.build_local_map(func, types, signatures)

    case Map.to_list(map) do
      [] ->
        []

      list ->
        names = Enum.map(list, fn {id, i} -> {i, id} end) |> Enum.sort()
        [{func_idx, names}]
    end
  end
end
