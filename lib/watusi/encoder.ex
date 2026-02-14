defmodule Watusi.Encoder do
  @moduledoc """
  Encoder for WebAssembly Binary Format.
  """
  alias Watusi.Instructions
  import Bitwise

  @control_flow_ops ["block", "loop", "if"]
  @branch_ops ["br", "br_if", "br_table"]
  @call_ops ["call", "call_indirect"]
  @global_ops ["global.get", "global.set"]

  @bulk_mem_ops [
    "memory.init",
    "data.drop",
    "memory.copy",
    "memory.fill",
    "table.init",
    "elem.drop",
    "table.copy"
  ]

  @memory_ops [
    "i32.load",
    "i64.load",
    "f32.load",
    "f64.load",
    "i32.store",
    "i64.store",
    "f32.store",
    "f64.store",
    "i32.load8_s",
    "i32.load8_u",
    "i32.load16_s",
    "i32.load16_u",
    "i64.load8_s",
    "i64.load8_u",
    "i64.load16_s",
    "i64.load16_u",
    "i64.load32_s",
    "i64.load32_u",
    "i32.store8",
    "i32.store16",
    "i64.store8",
    "v128.load",
    "v128.store",
    "i32.atomic.load",
    "i64.atomic.load",
    "i32.atomic.load8_u",
    "i32.atomic.load16_u",
    "i64.atomic.load8_u",
    "i64.atomic.load16_u",
    "i64.atomic.load32_u",
    "i32.atomic.store",
    "i64.atomic.store",
    "i32.atomic.store8",
    "i32.atomic.store16",
    "i64.atomic.store8",
    "i64.atomic.store16",
    "i64.atomic.store32",
    "i32.atomic.rmw.add",
    "i64.atomic.rmw.add",
    "i32.atomic.rmw8.add_u",
    "i32.atomic.rmw16.add_u",
    "i64.atomic.rmw8.add_u",
    "i64.atomic.rmw16.add_u",
    "i64.atomic.rmw32.add_u",
    "i32.atomic.rmw.sub",
    "i64.atomic.rmw.sub",
    "i32.atomic.rmw8.sub_u",
    "i32.atomic.rmw16.sub_u",
    "i64.atomic.rmw8.sub_u",
    "i64.atomic.rmw16.sub_u",
    "i64.atomic.rmw32.sub_u",
    "i32.atomic.rmw.and",
    "i64.atomic.rmw.and",
    "i32.atomic.rmw8.and_u",
    "i32.atomic.rmw16.and_u",
    "i64.atomic.rmw8.and_u",
    "i64.atomic.rmw16.and_u",
    "i64.atomic.rmw32.and_u",
    "i32.atomic.rmw.or",
    "i64.atomic.rmw.or",
    "i32.atomic.rmw8.or_u",
    "i32.atomic.rmw16.or_u",
    "i64.atomic.rmw8.or_u",
    "i64.atomic.rmw16.or_u",
    "i64.atomic.rmw32.or_u",
    "i32.atomic.rmw.xor",
    "i64.atomic.rmw.xor",
    "i32.atomic.rmw8.xor_u",
    "i32.atomic.rmw16.xor_u",
    "i64.atomic.rmw8.xor_u",
    "i64.atomic.rmw16.xor_u",
    "i64.atomic.rmw32.xor_u",
    "i32.atomic.rmw.xchg",
    "i64.atomic.rmw.xchg",
    "i32.atomic.rmw8.xchg_u",
    "i32.atomic.rmw16.xchg_u",
    "i64.atomic.rmw8.xchg_u",
    "i64.atomic.rmw16.xchg_u",
    "i64.atomic.rmw32.xchg_u",
    "i32.atomic.rmw.cmpxchg",
    "i64.atomic.rmw.cmpxchg",
    "i32.atomic.rmw8.cmpxchg_u",
    "i32.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw8.cmpxchg_u",
    "i64.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw32.cmpxchg_u",
    "memory.atomic.notify",
    "memory.atomic.wait32",
    "memory.atomic.wait64"
  ]

  @simd_ops [
    "v128.load",
    "v128.store",
    "v128.const",
    "i8x16.swizzle",
    "i32x4.splat",
    "i32x4.extract_lane",
    "i32x4.eq",
    "i32x4.lt_s",
    "v128.bitselect",
    "i32x4.neg",
    "i32x4.add",
    "i32x4.min_s",
    "f32x4.add"
  ]

  @atomic_ops [
    "memory.atomic.notify",
    "memory.atomic.wait32",
    "memory.atomic.wait64",
    "i32.atomic.load",
    "i64.atomic.load",
    "i32.atomic.load8_u",
    "i32.atomic.load16_u",
    "i64.atomic.load8_u",
    "i64.atomic.load16_u",
    "i64.atomic.load32_u",
    "i32.atomic.store",
    "i64.atomic.store",
    "i32.atomic.store8",
    "i32.atomic.store16",
    "i64.atomic.store8",
    "i64.atomic.store16",
    "i64.atomic.store32",
    "i32.atomic.rmw.add",
    "i64.atomic.rmw.add",
    "i32.atomic.rmw8.add_u",
    "i32.atomic.rmw16.add_u",
    "i64.atomic.rmw8.add_u",
    "i64.atomic.rmw16.add_u",
    "i64.atomic.rmw32.add_u",
    "i32.atomic.rmw.sub",
    "i64.atomic.rmw.sub",
    "i32.atomic.rmw8.sub_u",
    "i32.atomic.rmw16.sub_u",
    "i64.atomic.rmw8.sub_u",
    "i64.atomic.rmw16.sub_u",
    "i64.atomic.rmw32.sub_u",
    "i32.atomic.rmw.and",
    "i64.atomic.rmw.and",
    "i32.atomic.rmw8.and_u",
    "i32.atomic.rmw16.and_u",
    "i64.atomic.rmw8.and_u",
    "i64.atomic.rmw16.and_u",
    "i64.atomic.rmw32.and_u",
    "i32.atomic.rmw.or",
    "i64.atomic.rmw.or",
    "i32.atomic.rmw8.or_u",
    "i32.atomic.rmw16.or_u",
    "i64.atomic.rmw8.or_u",
    "i64.atomic.rmw16.or_u",
    "i64.atomic.rmw32.or_u",
    "i32.atomic.rmw.xor",
    "i64.atomic.rmw.xor",
    "i32.atomic.rmw8.xor_u",
    "i32.atomic.rmw16.xor_u",
    "i64.atomic.rmw8.xor_u",
    "i64.atomic.rmw16.xor_u",
    "i64.atomic.rmw32.xor_u",
    "i32.atomic.rmw.xchg",
    "i64.atomic.rmw.xchg",
    "i32.atomic.rmw8.xchg_u",
    "i32.atomic.rmw16.xchg_u",
    "i64.atomic.rmw8.xchg_u",
    "i64.atomic.rmw16.xchg_u",
    "i64.atomic.rmw32.xchg_u",
    "i32.atomic.rmw.cmpxchg",
    "i64.atomic.rmw.cmpxchg",
    "i32.atomic.rmw8.cmpxchg_u",
    "i32.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw8.cmpxchg_u",
    "i64.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw32.cmpxchg_u"
  ]

  # Guards for instruction categories
  defguardp is_control_flow(name) when name in @control_flow_ops
  defguardp is_branch(name) when name in @branch_ops
  defguardp is_call(name) when name in @call_ops
  defguardp is_global_op(name) when name in @global_ops

  defguardp is_bulk_mem(name) when name in @bulk_mem_ops

  defguardp is_mem_instr(name) when name in @memory_ops

  defguardp is_simd(name) when name in @simd_ops

  defguardp is_atomic(name) when name in @atomic_ops

  def encode([[{:keyword, "module"} | rest]], opts \\ []) do
    debug_names = Keyword.get(opts, :debug_names, false)

    module_id =
      case rest do
        [{:id, id} | _other] -> id
        _other -> nil
      end

    body =
      case rest do
        [{:id, _id} | tail] -> tail
        rest -> rest
      end

    header = <<0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00>>

    sections = group_sections(body)

    counts = resolve_counts(sections.imports)

    exports = collect_exports(body, counts)

    signatures = prepare_signatures(sections)

    ctx = %{
      local_map: %{},
      funcs: sections.funcs,
      imports: sections.imports,
      globals: sections.globals,
      sigs: signatures,
      types: sections.types,
      data: sections.data,
      elems: sections.elems,
      tables: sections.tables,
      memories: sections.memories
    }

    # Prepare specific sections

    elem_section = encode_section(9, encode_vector(sections.elems, &encode_elem(&1, ctx)))

    data_count_section = prepare_data_count_section(sections, needs_data_count?(sections))

    code_section = encode_section(10, encode_vector(sections.funcs, &encode_func_body(&1, ctx)))

    data_section = encode_section(11, encode_vector(sections.data, &encode_data(&1, ctx)))

    # Custom Name Section

    name_section =
      case debug_names do
        true -> encode_name_section(module_id, sections, counts)
        false -> []
      end

    IO.iodata_to_binary([
      header,
      encode_section(1, encode_vector(signatures, &encode_signature/1)),
      encode_section(
        2,
        encode_vector(sections.imports, &encode_import(&1, signatures, sections.types))
      ),
      encode_section(
        3,
        encode_vector(
          sections.funcs,
          fn f ->
            sig = extract_signature(f, sections.types)

            Enum.find_index(signatures, &(&1 == sig))
          end,
          &encode_u32/1
        )
      ),
      encode_section(4, encode_vector(sections.tables, &encode_table/1)),
      encode_section(5, encode_vector(sections.memories, &encode_memory/1)),
      encode_section(6, encode_vector(sections.globals, &encode_global(&1, ctx))),
      encode_export_section(exports, sections, signatures),
      encode_start_section(sections.starts, sections.funcs, sections.imports),
      elem_section,
      data_count_section,
      code_section,
      data_section,
      name_section
    ])
  end

  defp resolve_counts(imports) do
    %{
      func: count_imports(imports, "func"),
      table: count_imports(imports, "table"),
      memory: count_imports(imports, "memory"),
      global: count_imports(imports, "global")
    }
  end

  defp prepare_signatures(sections) do
    type_sigs = Enum.map(sections.types, &extract_raw_signature/1)

    (collect_import_signatures(sections.imports, sections.types) ++
       Enum.map(sections.funcs, &extract_signature(&1, sections.types)) ++
       type_sigs)
    |> Enum.uniq()
  end

  defp needs_data_count?(sections) do
    Enum.any?(sections.data, &passive_data?(&1)) or has_bulk_mem_instr?(sections.funcs)
  end

  defp prepare_data_count_section(sections, true) do
    case sections.data do
      [_ | _] = list -> encode_section(12, encode_u32(length(list)))
      _other -> []
    end
  end

  defp prepare_data_count_section(_sections, false), do: []

  defp passive_data?([{:keyword, "data"} | rest]) do
    # A data segment is passive if it doesn't have an offset expression
    !Enum.any?(rest, fn
      [{:keyword, _} | _] -> true
      _ -> false
    end)
  end

  defp has_bulk_mem_instr?(funcs) do
    Enum.any?(funcs, fn [_head | rest] ->
      instructions = collect_instructions(rest)

      Enum.any?(instructions, fn {:instr, name, _args, _labels} ->
        name in ["memory.init", "data.drop", "table.init", "elem.drop"]
      end)
    end)
  end

  defp group_sections(body) do
    grouped =
      Enum.group_by(body, fn
        [{:keyword, kind} | _] -> kind
        _ -> :other
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

  defp encode_export_section([], _sections, _sigs), do: []

  defp encode_export_section(exports, sections, _sigs) do
    encode_section(
      7,
      encode_vector(exports, fn {:export, name, kind, index_or_id} ->
        index =
          case kind do
            :func -> resolve_func_index(index_or_id, sections.funcs, sections.imports)
            :table -> resolve_table_index(index_or_id, sections.tables, sections.imports)
            :memory -> resolve_memory_index(index_or_id, sections.memories, sections.imports)
            :global -> resolve_global_index(index_or_id, sections.globals, sections.imports)
          end

        kind_byte =
          case kind do
            :func -> 0x00
            :table -> 0x01
            :memory -> 0x02
            :global -> 0x03
          end

        [encode_string(name), kind_byte, encode_u32(index)]
      end)
    )
  end

  defp encode_start_section([], _funcs, _imports), do: []

  defp encode_start_section([[{:keyword, "start"}, index_or_id] | _], funcs, imports) do
    encode_section(8, encode_u32(resolve_func_index(index_or_id, funcs, imports)))
  end

  defp count_imports(imports, kind) do
    Enum.count(imports, fn
      [{:keyword, "import"}, _, _, [{:keyword, ^kind} | _]] -> true
      _ -> false
    end)
  end

  defp collect_exports(body, counts) do
    {exports, _, _, _, _} =
      Enum.reduce(body, {[], 0, 0, 0, 0}, fn
        [{:keyword, "func"} | _] = f, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(f, :func, counts.func + f_i)
          {acc ++ shorthands, f_i + 1, t_i, m_i, g_i}

        [{:keyword, "table"} | _] = t, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(t, :table, counts.table + t_i)
          {acc ++ shorthands, f_i, t_i + 1, m_i, g_i}

        [{:keyword, "memory"} | _] = m, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(m, :memory, counts.memory + m_i)
          {acc ++ shorthands, f_i, t_i, m_i + 1, g_i}

        [{:keyword, "global"} | _] = g, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(g, :global, counts.global + g_i)
          {acc ++ shorthands, f_i, t_i, m_i, g_i + 1}

        [{:keyword, "export"}, {:string, name}, [{:keyword, kind}, index_or_id]],
        {acc, f_i, t_i, m_i, g_i} ->
          kind_atom = String.to_atom(kind)
          {acc ++ [{:export, name, kind_atom, index_or_id}], f_i, t_i, m_i, g_i}

        _, state ->
          state
      end)

    exports
  end

  defp collect_shorthands(item, kind, index) do
    item
    |> Enum.filter(fn
      [{:keyword, "export"}, {:string, _}] -> true
      _ -> false
    end)
    |> Enum.map(fn [{:keyword, "export"}, {:string, name}] ->
      {:export, name, kind, {:int, index}}
    end)
  end

  defp collect_import_signatures(imports, types) do
    Enum.flat_map(imports, fn
      [{:keyword, "import"}, _, _, [{:keyword, "func"} | rest]] ->
        [extract_signature([{:keyword, "func"} | rest], types)]

      _other ->
        []
    end)
  end

  defp encode_import(
         [{:keyword, "import"}, {:string, mod}, {:string, name}, [{:keyword, "func"} | rest]],
         signatures,
         types
       ) do
    sig = extract_signature([{:keyword, "func"} | rest], types)
    type_idx = Enum.find_index(signatures, &(&1 == sig))
    [encode_string(mod), encode_string(name), 0x00, encode_u32(type_idx)]
  end

  defp encode_import(
         [{:keyword, "import"}, {:string, mod}, {:string, name}, [{:keyword, "table"} | rest]],
         _,
         _
       ) do
    [encode_string(mod), encode_string(name), 0x01, 0x70, encode_limits(rest)]
  end

  defp encode_import(
         [{:keyword, "import"}, {:string, mod}, {:string, name}, [{:keyword, "memory"} | rest]],
         _,
         _
       ) do
    [encode_string(mod), encode_string(name), 0x02, encode_limits(rest)]
  end

  defp encode_import(
         [
           {:keyword, "import"},
           {:string, mod},
           {:string, name},
           [{:keyword, "global"}, type_desc]
         ],
         _,
         _
       ) do
    {type, mut} = extract_global_type(type_desc)
    [encode_string(mod), encode_string(name), 0x03, encode_valtype(type), mut]
  end

  defp encode_table([{:keyword, "table"} | rest]) do
    rest =
      Enum.reject(rest, fn
        {:id, _} -> true
        [{:keyword, "export"}, _] -> true
        _ -> false
      end)

    [0x70, encode_limits(rest)]
  end

  defp encode_memory([{:keyword, "memory"} | rest]) do
    rest =
      Enum.reject(rest, fn
        {:id, _} -> true
        [{:keyword, "export"}, _] -> true
        _ -> false
      end)

    encode_limits(rest)
  end

  defp encode_limits(tokens) do
    is_shared = Enum.any?(tokens, &match?({:keyword, "shared"}, &1))

    tokens =
      Enum.filter(tokens, fn
        {:int, _} -> true
        _ -> false
      end)

    case {tokens, is_shared} do
      {[{:int, min}], true} -> [0x03, encode_u32(min), encode_u32(min)]
      {[{:int, min}], false} -> [0x00, encode_u32(min)]
      {[{:int, min}, {:int, max}], true} -> [0x03, encode_u32(min), encode_u32(max)]
      {[{:int, min}, {:int, max}], false} -> [0x01, encode_u32(min), encode_u32(max)]
      _other -> raise "Invalid limits: #{inspect(tokens)}"
    end
  end

  defp encode_global([{:keyword, "global"} | rest], ctx) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        rest -> rest
      end

    {type_desc, rest} = List.pop_at(rest, 0)
    {type, mut} = extract_global_type(type_desc)
    instructions = collect_instructions(rest)
    [encode_valtype(type), mut, Enum.map(instructions, &encode_instruction(&1, ctx, [])), 0x0B]
  end

  defp extract_global_type({:keyword, t}), do: {t, 0x00}
  defp extract_global_type([{:keyword, "mut"}, {:keyword, t}]), do: {t, 0x01}

  defp encode_elem([{:keyword, "elem"} | rest], ctx) do
    {offset_instrs, rest} =
      case rest do
        [[{:keyword, "offset"} | expr] | tail] -> {collect_instructions(expr), tail}
        [[{:keyword, _} | _] = instr | tail] -> {collect_instructions([instr]), tail}
        _ -> raise "Invalid elem segment"
      end

    func_indices =
      Enum.map(rest, fn
        {:id, _} = id -> resolve_func_index(id, ctx.funcs, ctx.imports)
        {:int, i} -> i
      end)

    [
      0x00,
      Enum.map(offset_instrs, &encode_instruction(&1, ctx, [])),
      0x0B,
      encode_vector(func_indices, &encode_u32/1)
    ]
  end

  defp encode_data([{:keyword, "data"} | rest], ctx) do
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
        [0x01, encode_string(string)]

      _ ->
        {memidx, _} =
          case rest do
            [{:int, i} | _] -> {i, rest}
            _ -> {0, rest}
          end

        offset_expr =
          offset_expr_item |> collect_instructions() |> Enum.map(&encode_instruction(&1, ctx, []))

        [encode_u32(memidx), offset_expr, 0x0B, encode_string(string)]
    end
  end

  defp extract_signature([{:keyword, "func"} | rest] = func, types) do
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

  defp extract_raw_signature([{:keyword, "type"} | rest]) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        rest -> rest
      end

    case rest do
      [[{:keyword, "func"} | inner] | _other] ->
        extract_raw_signature([{:keyword, "func"} | inner])

      _other ->
        {[], []}
    end
  end

  defp extract_raw_signature([{:keyword, "func"} | rest]) do
    metadata =
      case rest do
        [{:id, _} | tail] -> tail
        rest -> rest
      end
      |> Enum.take_while(fn
        [{:keyword, k} | _] when k in ["param", "result", "local", "export"] -> true
        _other -> false
      end)

    params =
      metadata
      |> Enum.flat_map(fn
        [{:keyword, "param"} | ts] -> ts
        _other -> []
      end)
      |> Enum.reject(&match?({:id, _}, &1))
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

  defp encode_signature({params, results}) do
    [
      0x60,
      encode_vector(params, &encode_valtype/1),
      encode_vector(results, &encode_valtype/1)
    ]
  end

  defp encode_valtype(type), do: Instructions.valtype(type)

  defp encode_func_body([{:keyword, "func"} | rest] = func, ctx) do
    local_map = build_local_map(func)

    locals =
      Enum.flat_map(rest, fn
        [{:keyword, "local"} | ts] ->
          Enum.reject(ts, &match?({:id, _}, &1)) |> Enum.map(fn {:keyword, t} -> t end)

        _ ->
          []
      end)

    grouped_locals = group_locals(locals)

    encoded_locals =
      encode_vector(grouped_locals, fn {count, t} -> [encode_u32(count), encode_valtype(t)] end)

    instructions = collect_instructions(rest)
    func_ctx = %{ctx | local_map: local_map}
    encoded_instrs = Enum.map(instructions, &encode_instruction(&1, func_ctx, []))

    body = [encoded_locals, encoded_instrs, 0x0B]
    [encode_u32(IO.iodata_length(body)), body]
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

  defp build_local_map([{:keyword, "func"} | rest]) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        rest -> rest
      end

    {_final_idx, map} =
      Enum.reduce(rest, {0, %{}}, fn
        [{:keyword, kind} | ts], {idx, acc} when kind in ["param", "local"] ->
          {new_idx, new_acc} =
            Enum.reduce(ts, {idx, acc}, fn
              {:id, id}, {i, a} -> {i, Map.put(a, id, i)}
              {:keyword, _}, {i, a} -> {i + 1, a}
              _other, state -> state
            end)

          {new_idx, new_acc}

        _other, state ->
          state
      end)

    map
  end

  defp collect_instructions(rest, label_stack \\ []) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        rest -> rest
      end

    rest
    |> Enum.reject(fn
      [{:keyword, "export"}, _] -> true
      [{:keyword, k} | _] when k in ["param", "local"] -> true
      _ -> false
    end)
    |> do_collect_instructions([], label_stack)
  end

  defp do_collect_instructions([], acc, _labels), do: Enum.reverse(acc)

  defp do_collect_instructions([{:keyword, name} | rest], acc, labels)
       when is_control_flow(name) do
    # Flat Style control flow: (block $id (result i32) ...)
    {args, remaining} = collect_args(rest, [])

    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        _ -> {nil, args}
      end

    new_labels = [label | labels]

    do_collect_instructions(remaining, [{:instr, name, args, labels} | acc], new_labels)
  end

  defp do_collect_instructions([{:keyword, "end"} | rest], acc, labels) do
    # POP label
    new_labels =
      case labels do
        [_ | tail] -> tail
        [] -> []
      end

    do_collect_instructions(rest, [{:instr, "end", [], labels} | acc], new_labels)
  end

  defp do_collect_instructions([[{:keyword, name} | _] | rest], acc, labels)
       when name in ["type", "param", "local", "export", "then", "else"] do
    do_collect_instructions(rest, acc, labels)
  end

  defp do_collect_instructions([[{:keyword, "result"} | _] | rest], acc, labels) do
    # Result outside of control flow head is discarded (already handled in head)
    do_collect_instructions(rest, acc, labels)
  end

  defp do_collect_instructions([{:keyword, name} | rest], acc, labels) do
    {args, remaining} = collect_args(rest, [])
    do_collect_instructions(remaining, [{:instr, name, args, labels} | acc], labels)
  end

  defp do_collect_instructions([[{:keyword, name} | args] | rest], acc, labels)
       when is_control_flow(name) do
    # Folded Style control flow
    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        _ -> {nil, args}
      end

    new_labels = [label | labels]

    blocktype =
      case Enum.find(args, fn
             [{:keyword, "result"} | _] -> true
             _ -> false
           end) do
        [{:keyword, "result"}, {:keyword, type}] -> encode_valtype(type)
        _ -> 0x40
      end

    inner_rest =
      Enum.reject(args, fn
        [{:keyword, "result"} | _] -> true
        _ -> false
      end)

    {inner_instrs, cond_instrs} =
      case name do
        "if" ->
          {collect_if_instrs(inner_rest, new_labels), collect_if_condition(inner_rest, labels)}

        _ ->
          {collect_instructions(inner_rest, new_labels), []}
      end

    new_acc =
      [{:instr, "end", [], labels}] ++
        Enum.reverse(inner_instrs) ++
        [{:instr, name, [{:int, blocktype}], labels}] ++ Enum.reverse(cond_instrs) ++ acc

    do_collect_instructions(rest, new_acc, labels)
  end

  defp do_collect_instructions([[{:keyword, name} | args] | rest], acc, labels) do
    new_acc =
      [
        {:instr, name, filter_immediates(args), labels}
        | Enum.reverse(collect_folded_args(args, labels))
      ] ++ acc

    do_collect_instructions(rest, new_acc, labels)
  end

  defp do_collect_instructions([_ | rest], acc, labels),
    do: do_collect_instructions(rest, acc, labels)

  defp collect_if_condition(inner_rest, labels),
    do:
      Enum.reject(inner_rest, fn
        [{:keyword, k} | _] when k in ["then", "else"] -> true
        _ -> false
      end)
      |> collect_instructions(labels)

  defp collect_if_instrs(inner_rest, labels) do
    then_body =
      inner_rest
      |> Enum.find(&match?([{:keyword, "then"} | _other], &1))
      |> case do
        [{:keyword, "then"} | body] -> collect_instructions(body, labels)
        _other -> []
      end

    else_body =
      inner_rest
      |> Enum.find(&match?([{:keyword, "else"} | _other], &1))
      |> case do
        [{:keyword, "else"} | body] ->
          [{:instr, "else", [], labels} | collect_instructions(body, labels)]

        _other ->
          []
      end

    then_body ++ else_body
  end

  defp collect_folded_args(args, labels),
    do:
      Enum.filter(args, &match?([{:keyword, _} | _], &1))
      |> Enum.flat_map(&collect_instructions([&1], labels))

  @immediate_types [:int, :float, :id, :offset, :align]

  defp filter_immediates(args) do
    Enum.filter(args, fn
      {type, _} -> type in @immediate_types
      _other -> false
    end)
  end

  defp collect_args([{type, _} = arg | rest], acc) when type in @immediate_types,
    do: collect_args(rest, [arg | acc])

  defp collect_args([[{:keyword, "result"} | _] = arg | rest], acc),
    do: collect_args(rest, [arg | acc])

  defp collect_args(rest, acc), do: {Enum.reverse(acc), rest}

  defp encode_instruction({:instr, name, args, labels}, ctx, _) do
    case Instructions.opcode(name) do
      {:fc, op} -> [0xFC, LEB128.encode_unsigned(op) | encode_immediates(name, args, ctx, labels)]
      {:fd, op} -> [0xFD, LEB128.encode_unsigned(op) | encode_immediates(name, args, ctx, labels)]
      {:fe, op} -> [0xFE, LEB128.encode_unsigned(op) | encode_immediates(name, args, ctx, labels)]
      opcode -> [opcode | encode_immediates(name, args, ctx, labels)]
    end
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_mem_instr(name) do
    encode_mem_immediates(name, args)
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_control_flow(name) do
    encode_control_flow_immediates(args)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_bulk_mem(name) do
    encode_bulk_mem_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_simd(name) do
    encode_simd_immediates(name, args)
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_atomic(name) do
    encode_atomic_immediates(name, args)
  end

  defp encode_immediates(name, _args, _ctx, _labels)
       when name in ["memory.grow", "memory.size"] do
    [0x00]
  end

  defp encode_immediates(name, args, ctx, labels)
       when is_branch(name) or is_call(name) or name == "br_table" do
    encode_dynamic_immediates(name, args, ctx, labels)
  end

  defp encode_immediates(name, args, ctx, _labels) do
    Enum.map(args, &encode_arg(name, &1, ctx, []))
  end

  defp encode_dynamic_immediates("br_table", args, _ctx, labels) do
    indices =
      Enum.map(args, fn
        {:id, id} -> resolve_label(id, labels)
        {:int, i} -> i
      end)

    case indices do
      [] ->
        raise "br_table needs target"

      _ ->
        {ts, [d]} = Enum.split(indices, -1)
        [encode_vector(ts, &encode_u32/1), encode_u32(d)]
    end
  end

  defp encode_dynamic_immediates("call_indirect", args, ctx, _labels) do
    type_idx =
      case Enum.find(args, &match?([{:keyword, "type"}, _], &1)) do
        [{:keyword, "type"}, {:id, id}] -> resolve_type_id(id, ctx)
        [{:keyword, "type"}, {:int, i} | _] -> i
        _ -> 0
      end

    [encode_u32(type_idx), encode_u32(0)]
  end

  defp encode_dynamic_immediates(name, args, ctx, labels) when is_branch(name) or is_call(name) do
    Enum.map(args, &encode_arg(name, &1, ctx, labels))
  end

  defp encode_control_flow_immediates(args) do
    case args do
      [{:int, bt} | _] ->
        [bt]

      _ ->
        case Enum.find(args, &match?([{:keyword, "result"} | _], &1)) do
          [{:keyword, "result"}, {:keyword, type}] -> [encode_valtype(type)]
          _ -> [0x40]
        end
    end
  end

  defp encode_atomic_immediates(name, args)
       when name in ["memory.atomic.notify", "memory.atomic.wait32", "memory.atomic.wait64"] do
    # These also take a memidx immediate which is always 0 in WASM 1.0 threads
    [0x00 | encode_mem_immediates(name, args)]
  end

  defp encode_atomic_immediates(name, args) do
    encode_mem_immediates(name, args)
  end

  defp encode_simd_immediates(name, args) when name in ["v128.load", "v128.store"] do
    encode_mem_immediates(name, args)
  end

  defp encode_simd_immediates(name, args) when name in ["i32x4.extract_lane"] do
    case Enum.find(args, &match?({:int, _}, &1)) do
      {:int, lane} -> [lane]
      _other -> [0]
    end
  end

  defp encode_simd_immediates("v128.const", args) do
    # v128.const expects a shape keyword and then values.
    # We pack based on whether we see ints or floats and how many.
    values =
      args
      |> Enum.filter(fn
        {:int, _} -> true
        {:float, _} -> true
        _other -> false
      end)

    bytes =
      case values do
        # i8x16
        vals when length(vals) == 16 ->
          Enum.map(vals, fn {:int, v} -> <<v::8>> end)

        # i16x8
        vals when length(vals) == 8 ->
          Enum.map(vals, fn {:int, v} -> <<v::little-16>> end)

        # i32x4 or f32x4
        vals when length(vals) == 4 ->
          Enum.map(vals, fn
            {:int, v} -> <<v::little-32>>
            {:float, v} -> <<v::float-little-32>>
          end)

        # i64x2 or f64x2
        vals when length(vals) == 2 ->
          Enum.map(vals, fn
            {:int, v} -> <<v::little-64>>
            {:float, v} -> <<v::float-little-64>>
          end)

        _other ->
          raise "Invalid v128.const arguments: #{inspect(args)}"
      end
      |> IO.iodata_to_binary()

    [bytes]
  end

  defp encode_simd_immediates(_name, _args), do: []

  defp encode_bulk_mem_immediates("memory.init", args, ctx) do
    dataidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_data_index(id, ctx.data)
        {:int, i} -> i
      end

    [encode_u32(dataidx), 0x00]
  end

  defp encode_bulk_mem_immediates("data.drop", args, ctx) do
    dataidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_data_index(id, ctx.data)
        {:int, i} -> i
      end

    [encode_u32(dataidx)]
  end

  defp encode_bulk_mem_immediates("memory.copy", _, _), do: [0x00, 0x00]
  defp encode_bulk_mem_immediates("memory.fill", _, _), do: [0x00]

  defp encode_bulk_mem_immediates("table.init", args, ctx) do
    elemidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_elem_index(id, ctx.elems)
        {:int, i} -> i
      end

    tableidx =
      case Enum.at(args, 1) do
        {:id, id} -> resolve_table_index({:id, id}, ctx.tables, ctx.imports)
        {:int, i} -> i
        nil -> 0
      end

    [encode_u32(elemidx), encode_u32(tableidx)]
  end

  defp encode_bulk_mem_immediates("elem.drop", args, ctx) do
    elemidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_elem_index(id, ctx.elems)
        {:int, i} -> i
      end

    [encode_u32(elemidx)]
  end

  defp encode_bulk_mem_immediates("table.copy", args, ctx) do
    dst =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_table_index({:id, id}, ctx.tables, ctx.imports)
        {:int, i} -> i
        nil -> 0
      end

    src =
      case Enum.at(args, 1) do
        {:id, id} -> resolve_table_index({:id, id}, ctx.tables, ctx.imports)
        {:int, i} -> i
        nil -> 0
      end

    [encode_u32(dst), encode_u32(src)]
  end

  defp encode_mem_immediates(name, args) do
    offset =
      case Enum.find(args, &match?({:offset, _}, &1)) do
        {:offset, v} -> v
        _ -> 0
      end

    align =
      case Enum.find(args, &match?({:align, _}, &1)) do
        {:align, v} -> to_log2(v)
        _ -> natural_align(name)
      end

    [encode_u32(align), encode_u32(offset)]
  end

  defp natural_align(name) do
    cond do
      String.contains?(name, "v128") -> 4
      String.contains?(name, "64") -> 3
      String.contains?(name, "32") -> 2
      String.contains?(name, "16") -> 1
      String.contains?(name, "8") -> 0
      String.contains?(name, "notify") -> 2
      _other -> 0
    end
  end

  defp to_log2(1), do: 0
  defp to_log2(2), do: 1
  defp to_log2(4), do: 2
  defp to_log2(8), do: 3
  defp to_log2(16), do: 4
  defp to_log2(v), do: raise("Invalid align: #{v}")

  defp encode_arg(name, {:id, id}, _, labels) when is_branch(name),
    do: resolve_label(id, labels) |> encode_u32()

  defp encode_arg("i32.const", {:int, val}, _, _), do: val |> wrap_i32() |> LEB128.encode_signed()
  defp encode_arg("i64.const", {:int, val}, _, _), do: val |> wrap_i64() |> LEB128.encode_signed()

  defp encode_arg("f32.const", {:float, :neg_nan}, _, _), do: <<0, 0, 192, 255>>

  defp encode_arg("f32.const", {:float, {:nan, sign, payload}}, _, _) do
    bits = encode_sign(sign) <<< 31 ||| 0xFF <<< 23 ||| (payload &&& 0x7FFFFF)
    <<bits::little-32>>
  end

  defp encode_arg("f32.const", {:float, val}, _, _), do: encode_f32(val)
  defp encode_arg("f32.const", {:int, val}, _, _), do: encode_f32(val * 1.0)

  defp encode_arg("f64.const", {:float, :neg_nan}, _, _), do: <<0, 0, 0, 0, 0, 0, 248, 255>>

  defp encode_arg("f64.const", {:float, {:nan, sign, payload}}, _, _) do
    bits = encode_sign(sign) <<< 63 ||| 0x7FF <<< 52 ||| (payload &&& 0xFFFFFFFFFFFFF)
    <<bits::little-64>>
  end

  defp encode_arg("f64.const", {:float, val}, _, _), do: encode_f64(val)
  defp encode_arg("f64.const", {:int, val}, _, _), do: encode_f64(val * 1.0)

  defp encode_arg("call", {:id, id}, ctx, _),
    do: encode_u32(resolve_func_index({:id, id}, ctx.funcs, ctx.imports))

  defp encode_arg(name, {:id, id}, ctx, _) when is_global_op(name),
    do: encode_u32(resolve_global_index({:id, id}, ctx.globals, ctx.imports))

  defp encode_arg(_name, {:int, val}, _, _), do: encode_u32(val)
  defp encode_arg(_name, {:id, id}, ctx, _), do: encode_u32(Map.fetch!(ctx.local_map, id))

  defp wrap_i32(val) do
    <<signed::signed-32>> = <<val::unsigned-32>>
    signed
  end

  defp wrap_i64(val) do
    <<signed::signed-64>> = <<val::unsigned-64>>
    signed
  end

  defp encode_sign(-1), do: 1
  defp encode_sign(_), do: 0

  defp resolve_label(id, labels),
    do: Enum.find_index(labels, &(&1 == id)) || raise("Label not found: $#{id}")

  defp resolve_type_id(id, ctx) do
    idx =
      Enum.find_index(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
        raise("Type not found: $#{id}")

    sig = extract_raw_signature(Enum.at(ctx.types, idx))
    Enum.find_index(ctx.sigs, &(&1 == sig))
  end

  defp encode_f32(:infinity), do: <<0, 0, 128, 127>>
  defp encode_f32(:neg_infinity), do: <<0, 0, 128, 255>>
  defp encode_f32(:nan), do: <<0, 0, 192, 127>>
  defp encode_f32(f), do: <<f::float-little-size(32)>>

  defp encode_f64(:infinity), do: <<0, 0, 0, 0, 0, 0, 240, 127>>
  defp encode_f64(:neg_infinity), do: <<0, 0, 0, 0, 0, 0, 240, 255>>
  defp encode_f64(:nan), do: <<0, 0, 0, 0, 0, 0, 248, 127>>
  defp encode_f64(f), do: <<f::float-little-size(64)>>

  defp resolve_func_index({:int, i}, _, _), do: i
  defp resolve_func_index({:id, id}, fs, im), do: resolve_index(id, fs, im, "func")
  defp resolve_global_index({:int, i}, _, _), do: i
  defp resolve_global_index({:id, id}, gs, im), do: resolve_index(id, gs, im, "global")
  defp resolve_memory_index({:int, i}, _, _), do: i
  defp resolve_memory_index({:id, id}, ms, im), do: resolve_index(id, ms, im, "memory")
  defp resolve_table_index({:int, i}, _, _), do: i
  defp resolve_table_index({:id, id}, ts, im), do: resolve_index(id, ts, im, "table")

  defp resolve_index(id, list, imports, kind) do
    kind_imports =
      Enum.filter(imports, fn
        [{:keyword, "import"}, _, _, [{:keyword, ^kind} | _]] -> true
        _ -> false
      end)

    case find_id_in_imports(kind_imports, kind, id) do
      nil ->
        length(kind_imports) +
          (find_local_index(list, kind, id) || raise("ID not found: #{kind} $#{id}"))

      idx ->
        idx
    end
  end

  defp find_id_in_imports(imports, kind, id) do
    Enum.find_index(imports, fn
      [{:keyword, "import"}, _, _, [{:keyword, ^kind}, {:id, ^id} | _]] -> true
      _ -> false
    end)
  end

  defp find_local_index(ls, k, id),
    do: Enum.find_index(ls, &match?([{:keyword, ^k}, {:id, ^id} | _], &1))

  defp resolve_data_index(id, data),
    do:
      Enum.find_index(data, &match?([{:keyword, "data"}, {:id, ^id} | _], &1)) ||
        raise("Data ID not found: $#{id}")

  defp resolve_elem_index(id, elems),
    do:
      Enum.find_index(elems, &match?([{:keyword, "elem"}, {:id, ^id} | _], &1)) ||
        raise("Elem ID not found: $#{id}")

  defp encode_name_section(nil, %{imports: [], funcs: []}, _counts), do: []

  defp encode_name_section(module_id, sections, counts) do
    # Subsection 0: Module Name
    sub0 =
      case module_id do
        nil -> []
        id -> encode_name_subsection(0, encode_string(id))
      end

    # Subsection 1: Function Names
    func_names = collect_func_names(sections.imports, sections.funcs)

    sub1 =
      case func_names do
        [] -> []
        list -> encode_name_subsection(1, encode_vector(list, &encode_name_assoc/1))
      end

    # Subsection 2: Local Names
    local_names = collect_local_names(sections.funcs, counts.func)

    sub2 =
      case local_names do
        [] -> []
        list -> encode_name_subsection(2, encode_vector(list, &encode_indirect_name_assoc/1))
      end

    payload = [encode_string("name"), sub0, sub1, sub2]
    encode_section(0, payload)
  end

  defp encode_name_subsection(id, payload),
    do: [id, encode_u32(IO.iodata_length(payload)), payload]

  defp encode_name_assoc({idx, name}), do: [encode_u32(idx), encode_string(name)]

  defp encode_indirect_name_assoc({func_idx, map}) do
    [encode_u32(func_idx), encode_vector(map, &encode_name_assoc/1)]
  end

  defp collect_func_names(imports, funcs) do
    import_names =
      imports
      |> Enum.with_index()
      |> Enum.flat_map(fn
        {[{:keyword, "import"}, _, _, [{:keyword, "func"}, {:id, id} | _]], idx} -> [{idx, id}]
        _other -> []
      end)

    import_count = length(imports)

    local_names =
      funcs
      |> Enum.with_index()
      |> Enum.flat_map(fn
        {[{:keyword, "func"}, {:id, id} | _], idx} -> [{import_count + idx, id}]
        _other -> []
      end)

    import_names ++ local_names
  end

  defp collect_local_names(funcs, import_func_count) do
    funcs
    |> Enum.with_index()
    |> Enum.flat_map(fn {[{:keyword, "func"} | _] = func, idx} ->
      map = build_local_map(func)

      case Map.to_list(map) do
        [] ->
          []

        list ->
          [{import_func_count + idx, Enum.map(list, fn {id, i} -> {i, id} end) |> Enum.sort()}]
      end
    end)
  end

  defp encode_section(_id, payload) when payload in [[], [<<0>>, []]], do: []
  defp encode_section(id, p), do: [id, encode_u32(IO.iodata_length(p)), p]
  defp encode_vector(l, f), do: [encode_u32(length(l)), Enum.map(l, f)]
  defp encode_vector(l, m, f), do: [encode_u32(length(l)), Enum.map(l, m) |> Enum.map(f)]
  defp encode_u32(v), do: LEB128.encode_unsigned(v)
  defp encode_string(s), do: [encode_u32(byte_size(s)), s]
end
