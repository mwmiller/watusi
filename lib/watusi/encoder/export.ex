defmodule Watusi.Encoder.Export do
  @moduledoc false
  alias Watusi.Context
  alias Watusi.Encoder.Common

  defstruct [:name, :kind, :index]

  def collect(body, import_counts) do
    {exports, _indices, _import_indices} =
      Enum.reduce(
        body,
        {[], %{func: 0, table: 0, memory: 0, global: 0, tag: 0},
         %{func: 0, table: 0, memory: 0, global: 0, tag: 0}},
        &do_collect(&1, &2, import_counts)
      )

    Enum.reverse(exports)
  end

  def encode_section([], _ctx), do: []

  def encode_section(exports, ctx) do
    Common.encode_section(
      7,
      Common.encode_vector(exports, fn %__MODULE__{name: name, kind: kind, index: index_or_id} ->
        index = Context.resolve_index(ctx, kind, index_or_id)
        [Common.encode_string(name), kind_byte(kind), Common.encode_u32(index)]
      end)
    )
  end

  defp do_collect([{:keyword, "import"} | _] = item, {acc, indices, import_indices}, _counts) do
    {_mod, _name, kind, _rest} = Watusi.Encoder.Sections.normalize_import(item)
    kind_atom = String.to_atom(kind)
    idx = Map.fetch!(import_indices, kind_atom)
    shorthands = collect_shorthands(item, kind_atom, idx)
    {Enum.reverse(shorthands) ++ acc, indices, Map.update!(import_indices, kind_atom, &(&1 + 1))}
  end

  defp do_collect([{:keyword, kind} | _] = item, {acc, indices, import_indices}, counts)
       when kind in ["func", "table", "memory", "global", "tag"] do
    kind_atom = String.to_atom(kind)
    import_count = Map.fetch!(counts, kind_atom)
    local_idx = Map.fetch!(indices, kind_atom)
    idx = import_count + local_idx
    shorthands = collect_shorthands(item, kind_atom, idx)
    {Enum.reverse(shorthands) ++ acc, Map.update!(indices, kind_atom, &(&1 + 1)), import_indices}
  end

  defp do_collect([{:keyword, "export"}, {:string, name}, [{:keyword, kind} | rest]], state, _) do
    kind_atom = String.to_atom(kind)

    index_or_id =
      case rest do
        [{:id, id}] -> {:id, id}
        [{:int, i}] -> {:int, i}
        [] -> raise("Export missing index or identifier")
      end

    export = %__MODULE__{name: name, kind: kind_atom, index: index_or_id}
    {[export | elem(state, 0)], elem(state, 1), elem(state, 2)}
  end

  defp do_collect(_, state, _), do: state

  defp collect_shorthands(item, kind, idx) do
    item
    |> Enum.filter(&match?([{:keyword, "export"}, {:string, _}], &1))
    |> Enum.map(fn [{:keyword, "export"}, {:string, name}] ->
      %__MODULE__{name: name, kind: kind, index: {:int, idx}}
    end)
  end

  defp kind_byte(:func), do: 0x00
  defp kind_byte(:table), do: 0x01
  defp kind_byte(:memory), do: 0x02
  defp kind_byte(:global), do: 0x03
  defp kind_byte(:tag), do: 0x04
end
