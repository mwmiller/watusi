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
  @reftypes ["funcref", "anyfunc", "externref", "func"]

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

        elem_item = build_inline_elem_item(table_id, is_table_64, funcs)

        base_table = Enum.reject(rest, &match?([{:keyword, "elem"} | _], &1))
        cleaned_table = build_table_item_from_inline(base_table, funcs, rest)

        {cleaned_table, elem_item}
    end
  end

  defp build_inline_elem_item(table_id, is_64, funcs) do
    offset_const = if is_64, do: "i64.const", else: "i32.const"

    case table_id do
      nil ->
        [
          {:keyword, "elem"},
          [{:keyword, "offset"}, [{:keyword, offset_const}, {:int, 0}]] | funcs
        ]

      id ->
        [
          {:keyword, "elem"},
          [{:keyword, "table"}, id],
          [{:keyword, "offset"}, [{:keyword, offset_const}, {:int, 0}]] | funcs
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
        [extract_signature([{:keyword, "func"} | rest], types)]

      item ->
        case normalize_import(item) do
          {_, _, "func", rest} -> [extract_signature([{:keyword, "func"} | rest], types)]
          {_, _, "tag", rest} -> [extract_signature([{:keyword, "func"} | rest], types)]
          _ -> []
        end
    end)
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

  def extract_signature_index([{:keyword, "func"} | rest] = func, signatures, types) do
    case Enum.find(rest, &match?([{:keyword, "type"}, _], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        Enum.find_index(types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
          raise("Type not found: $#{id}")

      [{:keyword, "type"}, {:int, i}] ->
        i

      _ ->
        sig = extract_raw_signature(func)
        Enum.find_index(signatures, &(&1 == sig))
    end
  end

  def prepare_signatures(sections) do
    # signatures are unique types in the module
    type_sigs = Enum.map(sections.types, &extract_raw_signature/1)

    tag_sigs =
      Enum.map(sections.tags, fn [{:keyword, "tag"} | rest] ->
        extract_raw_signature([{:keyword, "func"} | rest])
      end)

    func_and_block_sigs =
      Enum.flat_map(sections.funcs, fn func ->
        [
          extract_signature(func, sections.types)
          | scan_for_signatures(func)
        ]
      end)

    existing_sigs = MapSet.new(type_sigs)

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
            _, acc -> acc
          end)

        {params_acc ++ params, results_acc}

      [{:keyword, "result"} | ts], {params_acc, results_acc} ->
        results =
          Enum.reduce(ts, [], fn
            {:keyword, t}, acc -> acc ++ [t]
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

  defp normalize_type({:keyword, t}), do: t
  defp normalize_type([{:keyword, "ref"}, _] = t), do: t
  defp normalize_type([{:keyword, "ref"}, {:keyword, "null"}, _] = t), do: t
  defp normalize_type(other), do: other

  def encode_signature({:struct, fields}, ctx) do
    # GC Struct type (0x5F) followed by vector of field types
    [0x5F, Common.encode_vector(fields, &encode_field(&1, ctx))]
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

  defp encode_field(field, ctx) do
    # A field can be (field i32) or (field (mut i32))
    {type, mut} =
      case field do
        [{:keyword, "field"} | rest] -> extract_field_type(rest)
        _ -> extract_field_type(field)
      end

    [encode_valtype(Instructions.valtype(type), ctx), mut]
  end

  defp extract_field_type([[{:keyword, "mut"}, {:keyword, type}] | _]), do: {type, 0x01}
  defp extract_field_type([{:keyword, type} | _]), do: {type, 0x00}
  defp extract_field_type({:keyword, type}), do: {type, 0x00}

  defp encode_valtype({:ref, node}, ctx), do: resolve_heap_type(node, ctx)
  defp encode_valtype(type, _ctx) when is_integer(type), do: [type]
  defp encode_valtype(type, _ctx) when is_binary(type), do: [Instructions.valtype(type)]

  defp resolve_heap_type([{:keyword, "ref"}, arg], ctx) do
    [0x6B, resolve_heap_type_arg(arg, ctx, false)]
  end

  defp resolve_heap_type([{:keyword, "ref"}, {:keyword, "null"}, arg], ctx) do
    [0x6C, resolve_heap_type_arg(arg, ctx, true)]
  end

  defp resolve_heap_type_arg(arg, ctx, nullable) do
    case arg do
      {:id, id} -> resolve_heap_type_id(id, ctx, nullable)
      {:int, i} -> LEB128.encode_signed(i)
      {:keyword, k} -> LEB128.encode_signed(heap_type_opcode(k))
    end
  end

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

  def encode_import(item, signatures, types) do
    ctx = %{signatures: signatures, types: types}
    encode_import(item, signatures, types, ctx)
  end

  def encode_import(item, signatures, types, ctx) do
    {mod, name, kind, rest} = normalize_import(item)

    [
      Common.encode_string(mod),
      Common.encode_string(name) | do_encode_import(kind, rest, signatures, types, ctx)
    ]
  end

  defp do_encode_import("func", rest, signatures, types, _ctx) do
    type_idx = extract_signature_index([{:keyword, "func"} | rest], signatures, types)
    [0x00, Common.encode_u32(type_idx)]
  end

  defp do_encode_import("table", rest, _signatures, _types, ctx) do
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

  defp do_encode_import("memory", rest, _signatures, _types, _ctx) do
    [0x02, encode_limits(rest)]
  end

  defp do_encode_import("global", rest, _signatures, _types, ctx) do
    type_desc =
      case rest do
        [{:id, _id}, desc] -> desc
        [desc] -> desc
      end

    {type, mut} = extract_global_type(type_desc)

    type_bytes =
      case type do
        {:ref, [{:keyword, "ref"}, {:id, id}]} ->
          # For globals with (ref $id), encode as pack_symbol directly
          pack_symbol(id, false)

        {:ref, [{:keyword, "ref"}, {:keyword, "null"}, {:id, id}]} ->
          # For globals with (ref null $id), encode as pack_symbol with nullable
          pack_symbol(id, true)

        {:ref, _} ->
          encode_valtype(type, ctx)

        _ ->
          encode_valtype(Instructions.valtype(type), ctx)
      end

    [0x03, type_bytes, mut]
  end

  defp do_encode_import("tag", rest, signatures, types, _ctx) do
    type_idx = extract_signature_index([{:keyword, "func"} | rest], signatures, types)
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
    # Table type defaults to funcref (0x70) if not specified
    type_node =
      Enum.find(rest, fn
        {:keyword, k} when k in @reftypes -> true
        [{:keyword, "ref"} | _] -> true
        _ -> false
      end) || "funcref"

    type_bytes = encode_valtype(Instructions.valtype(type_node), ctx)

    inline_elem =
      Enum.find(rest, fn
        [{:keyword, "elem"} | _] -> true
        _ -> false
      end)

    rest =
      Enum.reject(rest, fn
        {:id, _} -> true
        {:keyword, k} when k in @reftypes -> true
        [{:keyword, "export"}, _] -> true
        [{:keyword, "elem"} | _] -> true
        _ -> false
      end)

    limits =
      case {Enum.any?(rest, &match?({:int, _}, &1)), inline_elem} do
        {false, [{:keyword, "elem"} | funcs]} ->
          elem_count =
            Enum.count(funcs, fn
              {:id, _} -> true
              {:int, _} -> true
              _ -> false
            end)

          [0x01, Common.encode_u32(elem_count), Common.encode_u32(elem_count)]

        _ ->
          encode_limits(rest)
      end

    [type_bytes, limits]
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
  # Handle reference types in globals: (ref $t) or (mut (ref $t))
  defp extract_global_type([{:keyword, "ref"}, _] = t), do: {{:ref, t}, 0x00}

  defp extract_global_type([{:keyword, "mut"}, [{:keyword, "ref"}, _] = t]),
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
    rest =
      case rest do
        [{:id, _id} | tail] -> tail
        other -> other
      end

    {table_idx, rest} = resolve_elem_table_idx(rest, ctx)

    # 3. Extract offset if present. It might be explicit '(offset ...)' or a raw instruction.
    offset_node =
      Enum.find(rest, fn
        [{:keyword, "offset"} | _] -> true
        [{:keyword, name} | _] when is_offset_op(name) -> true
        _ -> false
      end)

    is_passive = is_nil(offset_node)
    is_declarative = Enum.any?(rest, &match?({:keyword, "declare"}, &1))

    reftype =
      Enum.find(rest, fn
        {:keyword, k} when k in @reftypes -> true
        [{:keyword, "ref"} | _] -> true
        _ -> false
      end) || "funcref"

    reftype_bytes = encode_valtype(Instructions.valtype(reftype), ctx)

    expr_nodes =
      rest
      |> extract_elem_expr_nodes()
      |> Enum.reject(&(&1 == offset_node))

    {indices, has_expr_payload} =
      resolve_elem_payload(rest, offset_node, expr_nodes, reftype, ctx)

    encoded_exprs =
      Common.encode_vector(expr_nodes, fn expr_node ->
        expr_instrs =
          [expr_node]
          |> InstrEncoder.collect_instructions(ctx)
          |> Enum.map(&InstrEncoder.encode_instruction(&1, ctx))

        [expr_instrs, 0x0B]
      end)

    reftype_str =
      case reftype do
        {:keyword, k} -> k
        k when is_binary(k) -> k
        _ -> "funcref"
      end

    needs_explicit_reftype = reftype_str not in ["funcref", "func", "anyfunc"]

    encode_elem_segment(
      {is_declarative, is_passive, has_expr_payload, table_idx, needs_explicit_reftype},
      reftype_bytes,
      encoded_exprs,
      indices,
      offset_node,
      ctx
    )
  end

  defp encode_elem_segment(
         {true, _, true, _, _},
         reftype_bytes,
         encoded_exprs,
         _indices,
         _offset_node,
         _ctx
       ) do
    [0x07, reftype_bytes, encoded_exprs]
  end

  defp encode_elem_segment(
         {true, _, false, _, _},
         _reftype_bytes,
         _encoded_exprs,
         indices,
         _offset_node,
         _ctx
       ) do
    [0x03, 0x00, Common.encode_vector(indices, &Common.encode_u32/1)]
  end

  defp encode_elem_segment(
         {false, true, true, _, _},
         reftype_bytes,
         encoded_exprs,
         _indices,
         _offset_node,
         _ctx
       ) do
    [0x05, reftype_bytes, encoded_exprs]
  end

  defp encode_elem_segment(
         {false, true, false, _, _},
         _reftype_bytes,
         _encoded_exprs,
         indices,
         _offset_node,
         _ctx
       ) do
    [0x01, 0x00, Common.encode_vector(indices, &Common.encode_u32/1)]
  end

  defp encode_elem_segment(
         {false, false, true, 0, false},
         _reftype_bytes,
         encoded_exprs,
         _indices,
         offset_node,
         ctx
       ) do
    offset_expr = extract_offset_expr(offset_node)
    offset_instrs = InstrEncoder.collect_instructions(offset_expr, ctx)

    [
      0x04,
      Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B,
      encoded_exprs
    ]
  end

  defp encode_elem_segment(
         {false, false, true, table_idx, _},
         reftype_bytes,
         encoded_exprs,
         _indices,
         offset_node,
         ctx
       ) do
    offset_expr = extract_offset_expr(offset_node)
    offset_instrs = InstrEncoder.collect_instructions(offset_expr, ctx)

    [
      0x06,
      Common.encode_u32(table_idx),
      Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B,
      reftype_bytes,
      encoded_exprs
    ]
  end

  defp encode_elem_segment(
         {false, false, false, table_idx, _},
         _reftype_bytes,
         _encoded_exprs,
         indices,
         offset_node,
         ctx
       ) do
    offset_expr = extract_offset_expr(offset_node)
    offset_instrs = InstrEncoder.collect_instructions(offset_expr, ctx)

    case table_idx do
      0 ->
        [
          0x00,
          Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
          0x0B,
          Common.encode_vector(indices, &Common.encode_u32/1)
        ]

      _ ->
        [
          0x02,
          Common.encode_u32(table_idx),
          Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
          0x0B,
          0x00,
          Common.encode_vector(indices, &Common.encode_u32/1)
        ]
    end
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
        {InstrEncoder.resolve_index(id, ctx.tables, ctx.imports, "table"), tail}

      [[{:keyword, "table"}, {:int, i}] | tail] ->
        {i, tail}

      [{:keyword, "table"}, {:id, id} | tail] ->
        {InstrEncoder.resolve_index(id, ctx.tables, ctx.imports, "table"), tail}

      [{:keyword, "table"}, {:int, i} | tail] ->
        {i, tail}

      [{:int, i} | tail] ->
        {i, tail}

      other ->
        {0, other}
    end
  end

  defp resolve_elem_payload(rest, offset_node, expr_nodes, reftype, ctx) do
    expr_ref_func_indices =
      Enum.flat_map(expr_nodes, fn
        [{:keyword, "ref.func"}, {:id, id}] ->
          [InstrEncoder.resolve_index(id, ctx.funcs, ctx.imports, "func")]

        [{:keyword, "ref.func"}, {:int, i}] ->
          [i]

        _ ->
          []
      end)

    reftype_str =
      case reftype do
        {:keyword, k} -> k
        k when is_binary(k) -> k
        _ -> "funcref"
      end

    is_legacy_funcref_expr =
      case {reftype_str in ["funcref", "func", "anyfunc"], expr_nodes, expr_ref_func_indices} do
        {true, [_ | _], [_ | _]} -> length(expr_nodes) == length(expr_ref_func_indices)
        _ -> false
      end

    has_expr_payload = expr_nodes != [] and not is_legacy_funcref_expr

    indices =
      rest
      |> Enum.reject(&(&1 == offset_node))
      |> Enum.flat_map(fn
        {:id, _} = id -> [InstrEncoder.resolve_index(id, ctx.funcs, ctx.imports, "func")]
        {:int, i} -> [i]
        _ -> []
      end)
      |> Kernel.++(expr_ref_func_indices)

    {indices, has_expr_payload}
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

  defp normalize_item_expr(_), do: nil

  def encode_func_body([{:keyword, "func"} | rest] = func, ctx) do
    # Function bodies consist of local declarations followed by instructions
    local_map = InstrEncoder.build_local_map(func, ctx.types, ctx.sigs)

    locals =
      Enum.flat_map(rest, fn
        [{:keyword, "local"} | ts] ->
          Enum.reject(ts, &match?({:id, _}, &1)) |> Enum.map(fn {:keyword, t} -> t end)

        _ ->
          []
      end)

    grouped_locals = group_locals(locals)

    encoded_locals =
      Common.encode_vector(grouped_locals, fn {count, t} ->
        [Common.encode_u32(count), Instructions.valtype(t)]
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
  def encode_name_section(nil, %{imports: [], funcs: []}, _counts, _signatures), do: []

  def encode_name_section(module_id, sections, counts, signatures) do
    sub0 =
      case module_id do
        nil -> []
        id -> encode_name_subsection(0, Common.encode_string(id))
      end

    func_names = collect_func_names(sections.imports, sections.funcs)

    sub1 =
      case func_names do
        [] -> []
        list -> encode_name_subsection(1, Common.encode_vector(list, &encode_name_assoc/1))
      end

    local_names = collect_local_names(sections.funcs, counts.func, sections.types, signatures)

    sub2 =
      case local_names do
        [] ->
          []

        list ->
          encode_name_subsection(2, Common.encode_vector(list, &encode_indirect_name_assoc/1))
      end

    payload = [Common.encode_string("name"), sub0, sub1, sub2]
    Common.encode_section(0, payload)
  end

  defp encode_name_subsection(id, payload),
    do: [id, Common.encode_u32(IO.iodata_length(payload)), payload]

  defp encode_name_assoc({idx, name}), do: [Common.encode_u32(idx), Common.encode_string(name)]

  defp encode_indirect_name_assoc({func_idx, map}),
    do: [Common.encode_u32(func_idx), Common.encode_vector(map, &encode_name_assoc/1)]

  defp collect_func_names(imports, funcs) do
    import_names = Enum.with_index(imports) |> Enum.flat_map(&extract_import_func_name/1)
    import_count = length(imports)

    local_names =
      Enum.with_index(funcs) |> Enum.flat_map(&extract_local_func_name(&1, import_count))

    import_names ++ local_names
  end

  defp extract_import_func_name({item, idx}) do
    case normalize_import(item) do
      {_, _, "func", rest} ->
        case Enum.find(rest, &match?({:id, _}, &1)) do
          {:id, id} -> [{idx, id}]
          _ -> []
        end

      _ ->
        []
    end
  end

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
