defmodule Watusi.Encoder do
  @moduledoc """
  WASM Binary Encoder coordinator.
  """
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Instructions, as: InstrEncoder
  alias Watusi.Encoder.Sections

  def encode([[{:keyword, "module"} | rest]], opts \\ []) do
    debug_names = Keyword.get(opts, :debug_names, false)

    # Identifiers for the module itself are optional
    module_id =
      case rest do
        [{:id, id} | _] -> id
        _ -> nil
      end

    body =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    header = <<0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00>>
    sections = Sections.group_sections(body)
    counts = resolve_counts(sections.imports)
    exports = collect_exports(body, counts)
    signatures = Sections.prepare_signatures(sections)

    # The context map is passed down to allow instructions to resolve identifiers
    # into numeric indices based on the full module state.
    ctx = %{
      local_map: %{},
      funcs: sections.funcs,
      imports: sections.imports,
      globals: sections.globals,
      tags: sections.tags,
      sigs: signatures,
      types: sections.types,
      data: sections.data,
      elems: sections.elems,
      tables: sections.tables,
      memories: sections.memories
    }

    # Section 9: Elements (Table initializers)
    elem_section =
      Common.encode_section(9, Common.encode_vector(sections.elems, &Sections.encode_elem(&1, ctx)))

    # Section 13: Exception Tags (from Exception Handling proposal)
    tag_section =
      Common.encode_section(
        13,
        Common.encode_vector(sections.tags, &Sections.encode_tag(&1, signatures, sections.types))
      )

    # Section 12: Data Count (Required for Bulk Memory validation)
    data_count_section = prepare_data_count_section(sections, needs_data_count?(sections, ctx))

    # Section 10: Function Bodies
    code_section =
      Common.encode_section(
        10,
        Common.encode_vector(sections.funcs, &Sections.encode_func_body(&1, ctx))
      )

    # Section 11: Data segments (Memory initializers)
    data_section =
      Common.encode_section(11, Common.encode_vector(sections.data, &Sections.encode_data(&1, ctx)))

    # Custom Section 0: Debug names (Optional)
    name_section =
      case debug_names do
        true -> Sections.encode_name_section(module_id, sections, counts)
        false -> []
      end

    # Sections must appear in a specific numeric order defined by the WASM spec
    IO.iodata_to_binary([
      header,
      Common.encode_section(1, Common.encode_vector(signatures, &Sections.encode_signature/1)),
      Common.encode_section(
        2,
        Common.encode_vector(sections.imports, &Sections.encode_import(&1, signatures, sections.types))
      ),
      Common.encode_section(
        3,
        Common.encode_vector(sections.funcs, fn f ->
          sig = Sections.extract_signature(f, sections.types)
          Enum.find_index(signatures, &(&1 == sig))
        end, &Common.encode_u32/1)
      ),
      Common.encode_section(4, Common.encode_vector(sections.tables, &Sections.encode_table/1)),
      Common.encode_section(5, Common.encode_vector(sections.memories, &Sections.encode_memory/1)),
      Common.encode_section(6, Common.encode_vector(sections.globals, &Sections.encode_global(&1, ctx))),
      tag_section,
      encode_export_section(exports, sections),
      encode_start_section(sections.starts, sections.funcs, sections.imports),
      elem_section,
      data_count_section,
      code_section,
      data_section,
      name_section
    ])
  end

  # Import counts are needed to calculate the base index for local declarations
  defp resolve_counts(imports) do
    %{
      func: count_imports(imports, "func"),
      table: count_imports(imports, "table"),
      memory: count_imports(imports, "memory"),
      global: count_imports(imports, "global"),
      tag: count_imports(imports, "tag")
    }
  end

  defp count_imports(imports, kind) do
    Enum.count(imports, fn item ->
      case Sections.normalize_import(item) do
        {_, _, ^kind, _} -> true
        _ -> false
      end
    end)
  end

  # Data count is required if any passive segments or bulk memory instructions are used
  defp needs_data_count?(sections, ctx) do
    Enum.any?(sections.data, &passive_data?(&1)) or has_bulk_mem_instr?(sections.funcs, ctx)
  end

  defp prepare_data_count_section(sections, true) do
    case sections.data do
      [_ | _] = list -> Common.encode_section(12, Common.encode_u32(length(list)))
      _ -> []
    end
  end

  defp prepare_data_count_section(_sections, false), do: []

  defp passive_data?([{:keyword, "data"} | rest]) do
    !Enum.any?(rest, fn
      [{:keyword, _} | _] -> true
      _ -> false
    end)
  end

  defp has_bulk_mem_instr?(funcs, ctx) do
    Enum.any?(funcs, fn [_head | rest] ->
      instructions = InstrEncoder.collect_instructions(rest, ctx)

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

  defp resolve_export_index(:tag, {:id, id}, sections),
    do: InstrEncoder.resolve_index(id, sections.tags, sections.imports, "tag")

  defp kind_to_byte(:func), do: 0x00
  defp kind_to_byte(:table), do: 0x01
  defp kind_to_byte(:memory), do: 0x02
  defp kind_to_byte(:global), do: 0x03
  defp kind_to_byte(:tag), do: 0x04

  defp encode_start_section([], _funcs, _imports), do: []

  defp encode_start_section([[{:keyword, "start"}, index_or_id] | _], funcs, imports) do
    index =
      case index_or_id do
        {:int, i} -> i
        {:id, id} -> InstrEncoder.resolve_index(id, funcs, imports, "func")
      end

    Common.encode_section(8, Common.encode_u32(index))
  end

  # Exports can be declared explicitly via (export ...) or inline via (func (export ...))
  defp collect_exports(body, counts) do
    {exports, _indices} =
      Enum.reduce(body, {[], %{func: 0, table: 0, memory: 0, global: 0, tag: 0}}, &do_collect_exports(&1, &2, counts))

    exports
  end

  defp do_collect_exports([{:keyword, kind} | _] = item, {acc, indices}, counts) when kind in ["func", "table", "memory", "global", "tag"] do
    case inline_import?(item) do
      true -> {acc, indices}
      false ->
        kind_atom = String.to_atom(kind)
        idx = Map.fetch!(counts, kind_atom) + Map.fetch!(indices, kind_atom)
        shorthands = collect_shorthands(item, kind_atom, idx)
        {acc ++ shorthands, Map.update!(indices, kind_atom, &(&1 + 1))}
    end
  end

  defp do_collect_exports([{:keyword, "export"}, {:string, name}, [{:keyword, kind}, index_or_id]], {acc, indices}, _counts) do
    kind_atom = String.to_atom(kind)
    {acc ++ [{:export, name, kind_atom, index_or_id}], indices}
  end

  defp do_collect_exports(_, state, _), do: state

  defp inline_import?(node) do
    Enum.any?(node, fn
      [{:keyword, "import"} | _] -> true
      _ -> false
    end)
  end

  defp collect_shorthands(item, kind, index) do
    item
    |> Enum.filter(&match?([{:keyword, "export"}, {:string, _}], &1))
    |> Enum.map(fn [{:keyword, "export"}, {:string, name}] ->
      {:export, name, kind, {:int, index}}
    end)
  end
end
