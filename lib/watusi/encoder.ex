defmodule Watusi.Encoder do
  @moduledoc """
  Encoder for WebAssembly Binary Format.
  """
  alias Watusi.Instructions
  import Bitwise

  # Guards for instruction categories
  defguardp is_control_flow(name) when name in ["block", "loop", "if"]
  defguardp is_branch(name) when name in ["br", "br_if", "br_table"]
  defguardp is_call(name) when name in ["call", "call_indirect"]
  defguardp is_global_op(name) when name in ["global.get", "global.set"]

  defguardp is_bulk_mem(name)
            when name in [
                   "memory.init",
                   "data.drop",
                   "memory.copy",
                   "memory.fill",
                   "table.init",
                   "elem.drop",
                   "table.copy"
                 ]

  defguardp is_mem_instr(name)
            when name in [
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
                   "i64.store16",
                   "i64.store32"
                 ]

  def encode([[{:keyword, "module"} | body]]) do
    header = <<0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00>>
    sections = group_sections(body)

    # Counts for index resolution
    counts = %{
      func: count_imports(sections.imports, "func"),
      table: count_imports(sections.imports, "table"),
      memory: count_imports(sections.imports, "memory"),
      global: count_imports(sections.imports, "global")
    }

    exports = collect_exports(body, counts)
    type_sigs = Enum.map(sections.types, &extract_raw_signature/1)

    signatures =
      (collect_import_signatures(sections.imports, sections.types) ++
         Enum.map(sections.funcs, &extract_signature(&1, sections.types)) ++
         type_sigs)
      |> Enum.uniq()

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

    elem_section = encode_section(9, encode_vector(sections.elems, &encode_elem(&1, ctx)))

    # Only include Data Count section if there are bulk memory instructions or passive data segments
    needs_data_count =
      Enum.any?(sections.data, &passive_data?(&1)) or has_bulk_mem_instr?(sections.funcs)

    data_count_section =
      case {needs_data_count, sections.data} do
        {true, [_ | _] = list} -> encode_section(12, encode_u32(length(list)))
        _ -> []
      end

    code_section = encode_section(10, encode_vector(sections.funcs, &encode_func_body(&1, ctx)))
    data_section = encode_section(11, encode_vector(sections.data, &encode_data(&1, ctx)))

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
      data_section
    ])
  end

  defp passive_data?([{:keyword, "data"} | rest]) do
    # A data segment is passive if it doesn't have an offset expression
    !Enum.any?(rest, fn
      [{:keyword, _} | _] -> true
      _ -> false
    end)
  end

  defp has_bulk_mem_instr?(funcs) do
    Enum.any?(funcs, fn [_ | rest] ->
      instructions = collect_instructions(rest)

      Enum.any?(instructions, fn {:instr, name, _, _} ->
        name in ["memory.init", "data.drop", "memory.copy", "memory.fill"]
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

      _ ->
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
    tokens =
      Enum.filter(tokens, fn
        {:int, _} -> true
        _ -> false
      end)

    case tokens do
      [{:int, min}] -> [0x00, encode_u32(min)]
      [{:int, min}, {:int, max}] -> [0x01, encode_u32(min), encode_u32(max)]
      _ -> raise "Invalid limits: #{inspect(tokens)}"
    end
  end

  defp encode_global([{:keyword, "global"} | rest], ctx) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        _ -> rest
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
      Enum.find(rest, fn
        {:string, _} -> true
        _ -> false
      end)
      |> elem(1)

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
        _ -> rest
      end

    case rest do
      [[{:keyword, "func"} | inner] | _] -> extract_raw_signature([{:keyword, "func"} | inner])
      _ -> {[], []}
    end
  end

  defp extract_raw_signature([{:keyword, "func"} | rest]) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        _ -> rest
      end

    # Only take metadata from the head of the function
    metadata =
      Enum.take_while(rest, fn
        [{:keyword, k} | _] when k in ["param", "result", "local", "export"] -> true
        _ -> false
      end)

    params =
      Enum.flat_map(metadata, fn
        [{:keyword, "param"} | ts] ->
          Enum.reject(ts, &match?({:id, _}, &1)) |> Enum.map(fn {:keyword, t} -> t end)

        _ ->
          []
      end)

    results =
      Enum.flat_map(metadata, fn
        [{:keyword, "result"} | ts] -> Enum.map(ts, fn {:keyword, t} -> t end)
        _ -> []
      end)

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
        _ -> [{1, type} | acc]
      end
    end)
    |> Enum.reverse()
  end

  defp build_local_map([{:keyword, "func"} | rest]) do
    params =
      Enum.flat_map(rest, fn
        [{:keyword, "param"} | ts] ->
          Enum.filter(ts, &match?({:id, _}, &1)) |> Enum.map(fn {:id, id} -> id end)

        _ ->
          []
      end)

    locals =
      Enum.flat_map(rest, fn
        [{:keyword, "local"} | ts] ->
          Enum.filter(ts, &match?({:id, _id}, &1)) |> Enum.map(fn {:id, id} -> id end)

        _ ->
          []
      end)

    (params ++ locals)
    |> Enum.with_index()
    |> Enum.into(%{}, fn {id, idx} -> {id, idx} end)
  end

  defp collect_instructions(rest, label_stack \\ []) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        _ -> rest
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
      case Enum.find(inner_rest, &match?([{:keyword, "then"} | _], &1)) do
        [{:keyword, "then"} | body] -> collect_instructions(body, labels)
        _ -> []
      end

    else_body =
      case Enum.find(inner_rest, &match?([{:keyword, "else"} | _], &1)) do
        [{:keyword, "else"} | body] ->
          [{:instr, "else", [], labels} | collect_instructions(body, labels)]

        _ ->
          []
      end

    then_body ++ else_body
  end

  defp collect_folded_args(args, labels),
    do:
      Enum.filter(args, &match?([{:keyword, _} | _], &1))
      |> Enum.flat_map(&collect_instructions([&1], labels))

  defp filter_immediates(args) do
    Enum.filter(args, fn
      {:int, _} -> true
      {:float, _} -> true
      {:id, _} -> true
      {:offset, _} -> true
      {:align, _} -> true
      _ -> false
    end)
  end

  defp collect_args([{:int, _} = arg | rest], acc), do: collect_args(rest, [arg | acc])
  defp collect_args([{:float, _} = arg | rest], acc), do: collect_args(rest, [arg | acc])
  defp collect_args([{:id, _} = arg | rest], acc), do: collect_args(rest, [arg | acc])
  defp collect_args([{:offset, _} = arg | rest], acc), do: collect_args(rest, [arg | acc])
  defp collect_args([{:align, _} = arg | rest], acc), do: collect_args(rest, [arg | acc])

  defp collect_args([[{:keyword, "result"} | _] = arg | rest], acc),
    do: collect_args(rest, [arg | acc])

  defp collect_args(rest, acc), do: {Enum.reverse(acc), rest}

  defp encode_instruction({:instr, name, args, labels}, ctx, _) do
    opcode = Instructions.opcode(name)
    [opcode | encode_immediates(name, args, ctx, labels)]
  end

  defp encode_immediates(name, args, ctx, labels) do
    cond do
      is_mem_instr(name) ->
        encode_mem_immediates(name, args)

      is_control_flow(name) ->
        encode_control_flow_immediates(args)

      is_bulk_mem(name) ->
        encode_bulk_mem_immediates(name, args, ctx)

      name in ["memory.grow", "memory.size"] ->
        [0x00]

      is_branch(name) or is_call(name) or name == "br_table" ->
        encode_dynamic_immediates(name, args, ctx, labels)

      true ->
        Enum.map(args, &encode_arg(name, &1, ctx, []))
    end
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
      String.contains?(name, "8") -> 0
      String.contains?(name, "16") -> 1
      String.contains?(name, "load32") or String.contains?(name, "store32") -> 2
      String.contains?(name, "64") -> 3
      String.contains?(name, "32") -> 2
      true -> 0
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

  defp encode_section(_id, payload) when payload in [[], [<<0>>, []]], do: []
  defp encode_section(id, p), do: [id, encode_u32(IO.iodata_length(p)), p]
  defp encode_vector(l, f), do: [encode_u32(length(l)), Enum.map(l, f)]
  defp encode_vector(l, m, f), do: [encode_u32(length(l)), Enum.map(l, m) |> Enum.map(f)]
  defp encode_u32(v), do: LEB128.encode_unsigned(v)
  defp encode_string(s), do: [encode_u32(byte_size(s)), s]
end
