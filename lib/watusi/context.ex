defmodule Watusi.Context do
  @moduledoc false

  defstruct [
    :funcs,
    :imports,
    :globals,
    :tags,
    :sigs,
    :types,
    :data,
    :elems,
    :tables,
    :memories,
    local_map: %{}
  ]

  def new(sections, signatures) do
    %__MODULE__{
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
  end

  def with_locals(%__MODULE__{} = ctx, local_map) do
    %{ctx | local_map: local_map}
  end

  def resolve_index(%__MODULE__{} = ctx, kind, id_or_int) when is_atom(kind) do
    case id_or_int do
      {:int, i} -> i
      {:id, id} -> resolve_by_id(ctx, kind, id)
    end
  end

  defp resolve_by_id(ctx, :func, id), do: find_index(id, ctx.funcs, ctx.imports, "func")
  defp resolve_by_id(ctx, :table, id), do: find_index(id, ctx.tables, ctx.imports, "table")
  defp resolve_by_id(ctx, :memory, id), do: find_index(id, ctx.memories, ctx.imports, "memory")
  defp resolve_by_id(ctx, :global, id), do: find_index(id, ctx.globals, ctx.imports, "global")
  defp resolve_by_id(ctx, :tag, id), do: find_index(id, ctx.tags, ctx.imports, "tag")
  defp resolve_by_id(ctx, :data, id), do: find_index(id, ctx.data, [], "data")
  defp resolve_by_id(ctx, :elem, id), do: find_index(id, ctx.elems, [], "elem")
  defp resolve_by_id(ctx, :local, id), do: Map.fetch!(ctx.local_map, id)
  defp resolve_by_id(ctx, :type, id), do: find_index(id, ctx.types, [], "type")

  defp find_index(id, items, imports, kind) do
    import_count = count_imports(imports, kind)

    case find_in_items(id, items) do
      {:ok, idx} -> import_count + idx
      :error -> find_in_imports(id, imports, kind)
    end
  end

  defp find_in_items(id, items) do
    items
    |> Enum.with_index()
    |> Enum.find_value(:error, fn {item, idx} ->
      if has_id?(item, id), do: {:ok, idx}
    end)
  end

  defp find_in_imports(id, imports, kind) do
    imports
    |> Enum.with_index()
    |> Enum.find_value(fn {item, idx} ->
      if import_matches?(item, id, kind), do: idx
    end) || raise("Identifier not found: $#{id}")
  end

  defp count_imports(imports, kind) do
    Enum.count(imports, &import_kind?(&1, kind))
  end

  defp has_id?([{:keyword, _}, {:id, id} | _], target_id), do: id == target_id
  defp has_id?(_, _), do: false

  defp import_matches?(item, id, kind) do
    case Watusi.Encoder.Sections.normalize_import(item) do
      {_, _, ^kind, rest} -> has_id?([{:keyword, kind} | rest], id)
      _ -> false
    end
  end

  defp import_kind?(item, kind) do
    case Watusi.Encoder.Sections.normalize_import(item) do
      {_, _, ^kind, _} -> true
      _ -> false
    end
  end
end
