defmodule Watusi.Encoder do
  @moduledoc """
  WASM Binary Encoder coordinator.
  """
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Instructions, as: InstrEncoder
  alias Watusi.Encoder.Sections

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
    sections = Sections.group_sections(body)
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
    elem_section =
      Common.encode_section(
        9,
        Common.encode_vector(sections.elems, &Sections.encode_elem(&1, ctx))
      )

    data_count_section = prepare_data_count_section(sections, needs_data_count?(sections))

    code_section =
      Common.encode_section(
        10,
        Common.encode_vector(sections.funcs, &Sections.encode_func_body(&1, ctx))
      )

    data_section =
      Common.encode_section(
        11,
        Common.encode_vector(sections.data, &Sections.encode_data(&1, ctx))
      )

    # Custom Name Section
    name_section =
      case debug_names do
        true -> Sections.encode_name_section(module_id, sections, counts)
        false -> []
      end

    IO.iodata_to_binary([
      header,
      Common.encode_section(1, Common.encode_vector(signatures, &Sections.encode_signature/1)),
      Common.encode_section(
        2,
        Common.encode_vector(
          sections.imports,
          &Sections.encode_import(&1, signatures, sections.types)
        )
      ),
      Common.encode_section(
        3,
        Common.encode_vector(
          sections.funcs,
          fn f ->
            sig = Sections.extract_signature(f, sections.types)
            Enum.find_index(signatures, &(&1 == sig))
          end,
          &Common.encode_u32/1
        )
      ),
      Common.encode_section(4, Common.encode_vector(sections.tables, &Sections.encode_table/1)),
      Common.encode_section(
        5,
        Common.encode_vector(sections.memories, &Sections.encode_memory/1)
      ),
      Common.encode_section(
        6,
        Common.encode_vector(sections.globals, &Sections.encode_global(&1, ctx))
      ),
      encode_export_section(exports, sections),
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

  defp count_imports(imports, kind) do
    Enum.count(imports, fn
      [{:keyword, "import"}, _, _, [{:keyword, ^kind} | _other]] -> true
      _other -> false
    end)
  end

  defp prepare_signatures(sections) do
    type_sigs = Enum.map(sections.types, &Sections.extract_raw_signature/1)

    (Sections.collect_import_signatures(sections.imports, sections.types) ++
       Enum.map(sections.funcs, &Sections.extract_signature(&1, sections.types)) ++
       type_sigs)
    |> Enum.uniq()
  end

  defp needs_data_count?(sections) do
    Enum.any?(sections.data, &passive_data?(&1)) or has_bulk_mem_instr?(sections.funcs)
  end

  defp prepare_data_count_section(sections, true) do
    case sections.data do
      [_ | _] = list -> Common.encode_section(12, Common.encode_u32(length(list)))
      _other -> []
    end
  end

  defp prepare_data_count_section(_sections, false), do: []

  defp passive_data?([{:keyword, "data"} | rest]) do
    !Enum.any?(rest, fn
      [{:keyword, _id} | _other] -> true
      _other -> false
    end)
  end

  defp has_bulk_mem_instr?(funcs) do
    Enum.any?(funcs, fn [_head | rest] ->
      instructions = InstrEncoder.collect_instructions(rest)

      Enum.any?(instructions, fn {:instr, name, _args, _labels} ->
        name in ["memory.init", "data.drop", "table.init", "elem.drop"]
      end)
    end)
  end

  defp encode_export_section([], _sections), do: []

  defp encode_export_section(exports, sections) do
    Common.encode_section(
      7,
      Common.encode_vector(exports, fn {:export, name, kind, index_or_id} ->
        index = resolve_export_index(kind, index_or_id, sections)

        [Common.encode_string(name), kind_to_byte(kind), Common.encode_u32(index)]
      end)
    )
  end

  defp resolve_export_index(_kind, {:int, i}, _sections), do: i

  defp resolve_export_index(:func, {:id, id}, sections),
    do: InstrEncoder.resolve_index(id, sections.funcs, sections.imports, "func")

  defp resolve_export_index(:table, {:id, id}, sections),
    do: InstrEncoder.resolve_index(id, sections.tables, sections.imports, "table")

  defp resolve_export_index(:memory, {:id, id}, sections),
    do: InstrEncoder.resolve_index(id, sections.memories, sections.imports, "memory")

  defp resolve_export_index(:global, {:id, id}, sections),
    do: InstrEncoder.resolve_index(id, sections.globals, sections.imports, "global")

  defp kind_to_byte(:func), do: 0x00

  defp kind_to_byte(:table), do: 0x01

  defp kind_to_byte(:memory), do: 0x02

  defp kind_to_byte(:global), do: 0x03

  defp encode_start_section([], _funcs, _imports), do: []

  defp encode_start_section([[{:keyword, "start"}, index_or_id] | _other], funcs, imports) do
    index =
      case index_or_id do
        {:int, i} -> i
        {:id, id} -> InstrEncoder.resolve_index(id, funcs, imports, "func")
      end

    Common.encode_section(8, Common.encode_u32(index))
  end

  defp collect_exports(body, counts) do
    {exports, _, _, _, _} =
      Enum.reduce(body, {[], 0, 0, 0, 0}, fn
        [{:keyword, "func"} | _other] = f, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(f, :func, counts.func + f_i)
          {acc ++ shorthands, f_i + 1, t_i, m_i, g_i}

        [{:keyword, "table"} | _other] = t, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(t, :table, counts.table + t_i)
          {acc ++ shorthands, f_i, t_i + 1, m_i, g_i}

        [{:keyword, "memory"} | _other] = m, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(m, :memory, counts.memory + m_i)
          {acc ++ shorthands, f_i, t_i, m_i + 1, g_i}

        [{:keyword, "global"} | _other] = g, {acc, f_i, t_i, m_i, g_i} ->
          shorthands = collect_shorthands(g, :global, counts.global + g_i)
          {acc ++ shorthands, f_i, t_i, m_i, g_i + 1}

        [{:keyword, "export"}, {:string, name}, [{:keyword, kind}, index_or_id]],
        {acc, f_i, t_i, m_i, g_i} ->
          kind_atom = String.to_atom(kind)
          {acc ++ [{:export, name, kind_atom, index_or_id}], f_i, t_i, m_i, g_i}

        _other, state ->
          state
      end)

    exports
  end

  defp collect_shorthands(item, kind, index) do
    item
    |> Enum.filter(fn
      [{:keyword, "export"}, {:string, _name}] -> true
      _other -> false
    end)
    |> Enum.map(fn [{:keyword, "export"}, {:string, name}] ->
      {:export, name, kind, {:int, index}}
    end)
  end
end
