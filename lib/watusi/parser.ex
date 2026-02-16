defmodule Watusi.Parser do
  @moduledoc false

  def parse(tokens) do
    {result, []} = do_parse(tokens, [])
    result
  end

  defp do_parse([], acc), do: {Enum.reverse(acc), []}

  defp do_parse([:lparen | rest], acc) do
    {sub_expr, remaining} = do_parse(rest, [])
    do_parse(remaining, [sub_expr | acc])
  end

  defp do_parse([:rparen | rest], acc) do
    {Enum.reverse(acc), rest}
  end

  defp do_parse([token | rest], acc) do
    do_parse(rest, [token | acc])
  end
end
