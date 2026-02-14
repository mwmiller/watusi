defmodule Watusi.Encoder.Sections do
  @moduledoc """
  WASM Section encoding logic.
  """
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Instructions, as: InstrEncoder
  alias Watusi.Instructions

  def group_sections(body) do
    grouped =
      Enum.group_by(body, fn
        [{:keyword, kind} | _other] -> kind
        _other -> :other
      end)

    %{
      imports: Map.get(grouped, "import", []),
      funcs: Map.get(grouped, "func", []),
      tables: Map.get(grouped, "table", []),
      memories: Map.get(grouped, "memory", []),
      globals: Map.get(grouped, "global", []),
      elems: Map.get(grouped, "elem", []),
      data: Map.get(grouped, "data", []),
      types: Map.get(grouped, "type", []),
      starts: Map.get(grouped, "start", [])
    }
  end

  def collect_import_signatures(imports, types) do
    Enum.flat_map(imports, fn
      [{:keyword, "import"}, _, _, [{:keyword, "func"} | rest]] ->
        [extract_signature([{:keyword, "func"} | rest], types)]

      _other ->
        []
    end)
  end

  def extract_signature([{:keyword, "func"} | rest] = func, types) do
    case Enum.find(rest, &match?([{:keyword, "type"}, _other], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        type_item =
          Enum.find(types, &match?([{:keyword, "type"}, {:id, ^id} | _other], &1)) ||
            raise("Type not found: $#{id}")

        extract_raw_signature(type_item)

      [{:keyword, "type"}, {:int, i}] ->
        extract_raw_signature(Enum.at(types, i))

      _other ->
        extract_raw_signature(func)
    end
  end

  def extract_raw_signature([{:keyword, "type"} | rest]) do
    rest =
      case rest do
        [{:id, _id} | tail] -> tail
        rest -> rest
      end

    case rest do
      [[{:keyword, "func"} | inner] | _other] ->
        extract_raw_signature([{:keyword, "func"} | inner])

      _other ->
        {[], []}
    end
  end

  def extract_raw_signature([{:keyword, "func"} | rest]) do
    metadata =
      case rest do
        [{:id, _id} | tail] -> tail
        rest -> rest
      end
      |> Enum.take_while(fn
        [{:keyword, k} | _other] when k in ["param", "result", "local", "export"] -> true
        _other -> false
      end)

    params =
      metadata
      |> Enum.flat_map(fn
        [{:keyword, "param"} | ts] -> ts
        _other -> []
      end)
      |> Enum.reject(&match?({:id, _id}, &1))
      |> Enum.map(fn {:keyword, t} -> t end)

    results =
      metadata
      |> Enum.flat_map(fn
        [{:keyword, "result"} | ts] -> ts
        _other -> []
      end)
      |> Enum.map(fn {:keyword, t} -> t end)

    {params, results}
  end

  def encode_signature({params, results}) do
    [
      0x60,
      Common.encode_vector(params, &Instructions.valtype/1),
      Common.encode_vector(results, &Instructions.valtype/1)
    ]
  end

  def encode_import(
        [{:keyword, "import"}, {:string, mod}, {:string, name}, [{:keyword, "func"} | rest]],
        signatures,
        types
      ) do
    sig = extract_signature([{:keyword, "func"} | rest], types)
    type_idx = Enum.find_index(signatures, &(&1 == sig))
    [Common.encode_string(mod), Common.encode_string(name), 0x00, Common.encode_u32(type_idx)]
  end

  def encode_import(
        [{:keyword, "import"}, {:string, mod}, {:string, name}, [{:keyword, "table"} | rest]],
        _sigs,
        _types
      ) do
    [Common.encode_string(mod), Common.encode_string(name), 0x01, 0x70, encode_limits(rest)]
  end

  def encode_import(
        [{:keyword, "import"}, {:string, mod}, {:string, name}, [{:keyword, "memory"} | rest]],
        _sigs,
        _types
      ) do
    [Common.encode_string(mod), Common.encode_string(name), 0x02, encode_limits(rest)]
  end

  def encode_import(
        [
          {:keyword, "import"},
          {:string, mod},
          {:string, name},
          [{:keyword, "global"}, type_desc]
        ],
        _sigs,
        _types
      ) do
    {type, mut} = extract_global_type(type_desc)
    [Common.encode_string(mod), Common.encode_string(name), 0x03, Instructions.valtype(type), mut]
  end

  def encode_table([{:keyword, "table"} | rest]) do
    rest =
      Enum.reject(rest, fn
        {:id, _id} -> true
        [{:keyword, "export"}, _other] -> true
        _other -> false
      end)

    [0x70, encode_limits(rest)]
  end

  def encode_memory([{:keyword, "memory"} | rest]) do
    rest =
      Enum.reject(rest, fn
        {:id, _id} -> true
        [{:keyword, "export"}, _other] -> true
        _other -> false
      end)

    encode_limits(rest)
  end

  def encode_limits(tokens) do
    is_shared = Enum.any?(tokens, &match?({:keyword, "shared"}, &1))

    tokens =
      Enum.filter(tokens, fn
        {:int, _id} -> true
        _other -> false
      end)

    case {tokens, is_shared} do
      {[{:int, min}], true} ->
        [0x03, Common.encode_u32(min), Common.encode_u32(min)]

      {[{:int, min}], false} ->
        [0x00, Common.encode_u32(min)]

      {[{:int, min}, {:int, max}], true} ->
        [0x03, Common.encode_u32(min), Common.encode_u32(max)]

      {[{:int, min}, {:int, max}], false} ->
        [0x01, Common.encode_u32(min), Common.encode_u32(max)]

      _other ->
        raise "Invalid limits: #{inspect(tokens)}"
    end
  end

  def encode_global([{:keyword, "global"} | rest], ctx) do
    rest =
      case rest do
        [{:id, _id} | tail] -> tail
        rest -> rest
      end

    {type_desc, rest} = List.pop_at(rest, 0)
    {type, mut} = extract_global_type(type_desc)
    instructions = InstrEncoder.collect_instructions(rest)

    [
      Instructions.valtype(type),
      mut,
      Enum.map(instructions, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B
    ]
  end

  defp extract_global_type({:keyword, t}), do: {t, 0x00}
  defp extract_global_type([{:keyword, "mut"}, {:keyword, t}]), do: {t, 0x01}

  def encode_elem([{:keyword, "elem"} | rest], ctx) do
    {offset_instrs, rest} =
      case rest do
        [[{:keyword, "offset"} | expr] | tail] ->
          {InstrEncoder.collect_instructions(expr), tail}

        [[{:keyword, _id} | _other] = instr | tail] ->
          {InstrEncoder.collect_instructions([instr]), tail}

        _other ->
          raise "Invalid elem segment"
      end

    func_indices =
      Enum.map(rest, fn
        {:id, id} -> InstrEncoder.resolve_index(id, ctx.funcs, ctx.imports, "func")
        {:int, i} -> i
      end)

    [
      0x00,
      Enum.map(offset_instrs, &InstrEncoder.encode_instruction(&1, ctx)),
      0x0B,
      Common.encode_vector(func_indices, &Common.encode_u32/1)
    ]
  end

  def encode_data([{:keyword, "data"} | rest], ctx) do
    offset_expr_item =
      Enum.find(rest, fn
        [{:keyword, _id} | _other] -> true
        _other -> false
      end)

    string =
      rest
      |> Enum.filter(&match?({:string, _s}, &1))
      |> Enum.map_join("", fn {:string, s} -> s end)

    case offset_expr_item do
      nil ->
        [0x01, Common.encode_string(string)]

      _other ->
        {memidx, _} =
          case rest do
            [{:int, i} | _other] -> {i, rest}
            _other -> {0, rest}
          end

        offset_expr =
          offset_expr_item
          |> InstrEncoder.collect_instructions()
          |> Enum.map(&InstrEncoder.encode_instruction(&1, ctx))

        [Common.encode_u32(memidx), offset_expr, 0x0B, Common.encode_string(string)]
    end
  end

  def encode_func_body([{:keyword, "func"} | rest] = func, ctx) do
    local_map = InstrEncoder.build_local_map(func)

    locals =
      Enum.flat_map(rest, fn
        [{:keyword, "local"} | ts] ->
          Enum.reject(ts, &match?({:id, _id}, &1)) |> Enum.map(fn {:keyword, t} -> t end)

        _other ->
          []
      end)

    grouped_locals = group_locals(locals)

    encoded_locals =
      Common.encode_vector(grouped_locals, fn {count, t} ->
        [Common.encode_u32(count), Instructions.valtype(t)]
      end)

    instructions = InstrEncoder.collect_instructions(rest)
    func_ctx = %{ctx | local_map: local_map}
    encoded_instrs = Enum.map(instructions, &InstrEncoder.encode_instruction(&1, func_ctx))

    body = [encoded_locals, encoded_instrs, 0x0B]
    [Common.encode_u32(IO.iodata_length(body)), body]
  end

  defp group_locals([]), do: []

  defp group_locals([first | rest]) do
    Enum.reduce(rest, [{1, first}], fn type, acc ->
      case acc do
        [{count, ^type} | tail] -> [{count + 1, type} | tail]
        _other -> [{1, type} | acc]
      end
    end)
    |> Enum.reverse()
  end

  # Name section logic
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
        {[{:keyword, "import"}, _, _, [{:keyword, "func"}, {:id, id} | _other]], idx} ->
          [{idx, id}]

        _other ->
          []
      end)

    import_count = length(imports)

    local_names =
      funcs
      |> Enum.with_index()
      |> Enum.flat_map(fn
        {[{:keyword, "func"}, {:id, id} | _other], idx} -> [{import_count + idx, id}]
        _other -> []
      end)

    import_names ++ local_names
  end

  defp collect_local_names(funcs, import_func_count) do
    funcs
    |> Enum.with_index()
    |> Enum.flat_map(fn {[{:keyword, "func"} | _other] = func, idx} ->
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
