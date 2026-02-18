defmodule Watusi.Encoder.Signature do
  @moduledoc false

  def extract_raw([{:keyword, "type"} | rest]) do
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    case rest do
      [[{:keyword, "func"} | inner] | _] -> extract_raw([{:keyword, "func"} | inner])
      [[{:keyword, "struct"} | inner] | _] -> {:struct, inner}
      [[{:keyword, "array"} | inner] | _] -> {:array, inner}
      _ -> {[], []}
    end
  end

  def extract_raw([{:keyword, "func"} | rest]) do
    metadata =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end
      |> Enum.take_while(fn
        [{:keyword, k} | _] when k in ["param", "result", "local", "export"] -> true
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
  defp normalize_type([{:keyword, "ref"}, {:keyword, t}]), do: "ref_#{t}"
  defp normalize_type([{:keyword, "ref"}, {:int, i}]), do: {:ref, i}
  defp normalize_type([{:keyword, "ref"}, {:id, id}]), do: {:ref, id}
  defp normalize_type([{:keyword, "ref"}, "null", {:keyword, t}]), do: "ref_null_#{t}"
  defp normalize_type([{:keyword, "ref"}, "null", {:int, i}]), do: {:ref_null, i}
  defp normalize_type([{:keyword, "ref"}, "null", {:id, id}]), do: {:ref_null, id}
  defp normalize_type(other), do: other

  def extract([{:keyword, "func"} | rest] = _func, types) do
    case Enum.find(rest, &match?([{:keyword, "type"}, _], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        type_item =
          Enum.find(types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
            raise("Type not found: $#{id}")

        extract_raw(type_item)

      [{:keyword, "type"}, {:int, i}] ->
        extract_raw(Enum.at(types, i))

      nil ->
        extract_inline(rest)
    end
  end

  def extract_index([{:keyword, "func"} | rest] = func, signatures, types) do
    case Enum.find(rest, &match?([{:keyword, "type"}, _], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        Enum.find_index(types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
          raise("Type not found: $#{id}")

      [{:keyword, "type"}, {:int, i}] ->
        i

      _ ->
        sig = extract(func, types)
        Enum.find_index(signatures, &(&1 == sig))
    end
  end

  def extract_inline(tokens) do
    params = collect_params(tokens)
    results = collect_results(tokens)
    {params, results}
  end

  defp collect_params(tokens) do
    tokens
    |> Enum.filter(&match?([{:keyword, "param"} | _], &1))
    |> Enum.flat_map(&extract_types/1)
  end

  defp collect_results(tokens) do
    tokens
    |> Enum.filter(&match?([{:keyword, "result"} | _], &1))
    |> Enum.flat_map(&extract_types/1)
  end

  defp extract_types([{:keyword, _} | rest]) do
    Enum.filter(rest, &match?({:keyword, _}, &1))
    |> Enum.map(fn {:keyword, t} -> t end)
  end
end
