defmodule Watusi.Encoder.Sections do
  @moduledoc """
  WASM Section encoding logic.
  """
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Instructions, as: InstrEncoder
  alias Watusi.Instructions
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
        extract_raw_signature(Enum.at(types, i))

      _ ->
        extract_raw_signature(func)
    end
  end

  def prepare_signatures(sections) do
    # signatures are unique types in the module
    type_sigs = Enum.map(sections.types, &extract_raw_signature/1)
    tag_sigs = Enum.map(sections.tags, &extract_raw_signature([{:keyword, "func"} | &1]))

    (collect_import_signatures(sections.imports, sections.types) ++
       Enum.map(sections.funcs, &extract_signature(&1, sections.types)) ++
       type_sigs ++ tag_sigs)
    |> Enum.uniq()
  end

  def extract_raw_signature([{:keyword, "type"} | rest]) do
    # Skip optional ID
    rest = case rest do [{:id, _} | tail] -> tail; other -> other end

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
      |> Enum.map(fn {:keyword, t} -> t end)

    results =
      metadata
      |> Enum.flat_map(fn
        [{:keyword, "result"} | ts] -> ts
        _ -> []
      end)
      |> Enum.map(fn {:keyword, t} -> t end)

    {params, results}
  end

  def encode_signature({:struct, fields}) do
    # GC Struct type (0x5F) followed by vector of field types
    [0x5F, Common.encode_vector(fields, &encode_field/1)]
  end

  def encode_signature({:array, field}) do
    # GC Array type (0x5E) followed by a single field type
    [0x5E, encode_field(field)]
  end

  def encode_signature({params, results}) do
    # Standard Function type (0x60)
    [
      0x60,
      Common.encode_vector(params, &Instructions.valtype/1),
      Common.encode_vector(results, &Instructions.valtype/1)
    ]
  end

  defp encode_field(field) do
    # A field can be (field i32) or (field (mut i32))
    {type, mut} =
      case field do
        [{:keyword, "field"} | rest] -> extract_field_type(rest)
        _ -> extract_field_type(field)
      end

    [Instructions.valtype(type), mut]
  end

  defp extract_field_type([[{:keyword, "mut"}, {:keyword, type}] | _]), do: {type, 0x01}
  defp extract_field_type([{:keyword, type} | _]), do: {type, 0x00}
  defp extract_field_type({:keyword, type}), do: {type, 0x00}

  def encode_import(item, signatures, types) do
    {mod, name, kind, rest} = normalize_import(item)

    case kind do
      "func" ->
        sig = extract_signature([{:keyword, "func"} | rest], types)
        type_idx = Enum.find_index(signatures, &(&1 == sig))
        [Common.encode_string(mod), Common.encode_string(name), 0x00, Common.encode_u32(type_idx)]

      "table" ->
        type_keyword = Enum.find(rest, &(&1 in @reftypes)) || "funcref"
        [Common.encode_string(mod), Common.encode_string(name), 0x01, Instructions.valtype(type_keyword), encode_limits(rest)]

      "memory" ->
        [Common.encode_string(mod), Common.encode_string(name), 0x02, encode_limits(rest)]

      "global" ->
        type_desc =
          case rest do
            [{:id, _id}, desc] -> desc
            [desc] -> desc
          end

        {type, mut} = extract_global_type(type_desc)

        [
          Common.encode_string(mod),
          Common.encode_string(name),
          0x03,
          Instructions.valtype(type),
          mut
        ]

      "tag" ->
        sig = extract_signature([{:keyword, "func"} | rest], types)
        type_idx = Enum.find_index(signatures, &(&1 == sig))
        [Common.encode_string(mod), Common.encode_string(name), 0x04, 0x00, Common.encode_u32(type_idx)]
    end
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

  def encode_table([{:keyword, "table"} | rest]) do
    # Table type defaults to funcref (0x70) if not specified
    type_keyword =
      Enum.find(rest, fn
        {:keyword, k} when k in @reftypes -> true
        _ -> false
      end)

    type_byte =
      case type_keyword do
        {:keyword, k} -> Instructions.valtype(k)
        _ -> 0x70
      end

    rest =
      Enum.reject(rest, fn
        {:id, _} -> true
        {:keyword, k} when k in @reftypes -> true
        [{:keyword, "export"}, _] -> true
        _ -> false
      end)

    [type_byte, encode_limits(rest)]
  end

  def encode_memory([{:keyword, "memory"} | rest]) do
    rest =
      Enum.reject(rest, fn
        {:id, _} -> true
        [{:keyword, "export"}, _] -> true
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

    {type_desc, rest} = List.pop_at(rest, 0)
    {type, mut} = extract_global_type(type_desc)
    instructions = InstrEncoder.collect_instructions(rest, ctx)

    [
      Instructions.valtype(type),
      mut,
      Enum.map(instructions, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B
    ]
  end

  defp extract_global_type({:keyword, t}), do: {t, 0x00}
  defp extract_global_type([{:keyword, "mut"}, {:keyword, t}]), do: {t, 0x01}

  def encode_tag([{:keyword, "tag"} | rest], signatures, _types) do
    # Tags (for Exception Handling) identify a signature for their payload
    sig = extract_raw_signature([{:keyword, "func"} | rest])
    type_idx = Enum.find_index(signatures, &(&1 == sig)) || raise("Type not found for tag: #{inspect(sig)}")
    [0x00, Common.encode_u32(type_idx)]
  end

  def encode_elem([{:keyword, "elem"} | rest], ctx) do
    # Elements (Table initializers) can be active, passive, or declarative
    rest = case rest do [{:id, _id} | tail] -> tail; other -> other end

    {table_idx, rest} =
      case rest do
        [{:keyword, "table"}, {:id, id} | tail] -> {InstrEncoder.resolve_index(id, ctx.tables, ctx.imports, "table"), tail}
        [{:keyword, "table"}, {:int, i} | tail] -> {i, tail}
        [{:int, i} | tail] -> {i, tail}
        other -> {0, other}
      end

    # 3. Extract offset if present. It might be explicit '(offset ...)' or a raw instruction.
    offset_node =
      Enum.find(rest, fn
        [{:keyword, "offset"} | _] -> true
        [{:keyword, name} | _] when name in ["i32.const", "i64.const", "global.get"] -> true
        _ -> false
      end)

    is_passive = is_nil(offset_node)

    # 4. Extract function identifiers (these are our payload)
    # We filter out the offset node and any keywords like 'func'
    indices =
      rest
      |> Enum.reject(&(&1 == offset_node))
      |> Enum.flat_map(fn
        {:id, _} = id -> [InstrEncoder.resolve_index(id, ctx.funcs, ctx.imports, "func")]
        {:int, i} -> [i]
        _ -> []
      end)

    case is_passive do
      true ->
        # flag 0x01: passive segment with function indices
        [0x01, 0x00, Common.encode_vector(indices, &Common.encode_u32/1)]

      false ->
        # Active segments use flag 0x00 for table 0 or 0x02 for other tables
        offset_expr =
          case offset_node do
            [{:keyword, "offset"} | expr] -> expr
            other -> [other]
          end

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
              Common.encode_vector(indices, &Common.encode_u32/1)
            ]
        end
    end
  end

  def encode_data([{:keyword, "data"} | rest], ctx) do
    # Data segments (Memory initializers) can be active or passive
    offset_expr_item =
      Enum.find(rest, fn
        [{:keyword, _} | _] -> true
        _ -> false
      end)

    string =
      rest
      |> Enum.filter(&match?({:string, _}, &1))
      |> Enum.map_join("", fn {:string, s} -> s end)

    case offset_expr_item do
      nil ->
        # flag 0x01: passive segment
        [0x01, Common.encode_string(string)]

      _ ->
        # Active segment
        {memidx, _} =
          case rest do
            [{:int, i} | _] -> {i, rest}
            _ -> {0, rest}
          end

        offset_expr =
          offset_expr_item
          |> InstrEncoder.collect_instructions(ctx)
          |> Enum.map(&InstrEncoder.encode_instruction(&1, ctx))

        [Common.encode_u32(memidx), offset_expr, 0x0B, Common.encode_string(string)]
    end
  end

  def encode_func_body([{:keyword, "func"} | rest] = func, ctx) do
    # Function bodies consist of local declarations followed by instructions
    local_map = InstrEncoder.build_local_map(func)

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
    Enum.reduce(rest, [{1, first}], fn type, acc ->
      case acc do
        [{count, ^type} | tail] -> [{count + 1, type} | tail]
        _ -> [{1, type} | acc]
      end
    end)
    |> Enum.reverse()
  end

  # The Custom (name) section allows debuggers to show symbolic names for indices
  def encode_name_section(nil, %{imports: [], funcs: []}, _counts), do: []

  def encode_name_section(module_id, sections, counts) do
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

    local_names = collect_local_names(sections.funcs, counts.func)

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
    import_names =
      imports
      |> Enum.with_index()
      |> Enum.flat_map(fn
        {item, idx} ->
          case normalize_import(item) do
            {_, _, "func", rest} ->
              case Enum.find(rest, &match?({:id, _}, &1)) do
                {:id, id} -> [{idx, id}]
                _ -> []
              end

            _ ->
              []
          end
      end)

    import_count = length(imports)

    local_names =
      funcs
      |> Enum.with_index()
      |> Enum.flat_map(fn
        {[{:keyword, "func"}, {:id, id} | _], idx} -> [{import_count + idx, id}]
        _ -> []
      end)

    import_names ++ local_names
  end

  defp collect_local_names(funcs, import_func_count) do
    funcs
    |> Enum.with_index()
    |> Enum.flat_map(fn {[{:keyword, "func"} | _] = func, idx} ->
      map = InstrEncoder.build_local_map(func)

      case Map.to_list(map) do
        [] ->
          []

        list ->
          [{import_func_count + idx, Enum.map(list, fn {id, i} -> {i, id} end) |> Enum.sort()}]
      end
    end)
  end
end
