defmodule SpecExtractor do
  def run(wast_path, output_dir) do
    File.mkdir_p!(Path.join(output_dir, "ok"))
    File.mkdir_p!(Path.join(output_dir, "fail"))
    content = File.read!(wast_path)
    # We use the existing lexer to find top-level (module ...) blocks
    tokens = Watusi.Lexer.tokenize(content)
    extract_modules(tokens, output_dir, 0)
  end

  defp extract_modules([], _, _), do: :ok

  defp extract_modules([:lparen, {:keyword, "assert_invalid"} | rest], dir, count) do
    {[_lparen, _keyword | module_tokens], remaining} = collect_sexpr(rest, [{:keyword, "assert_invalid"}, :lparen], 1)
    process_nested_module(module_tokens, dir, count, "fail")
    extract_modules(remaining, dir, count + 1)
  end

  defp extract_modules([:lparen, {:keyword, "assert_malformed"} | rest], dir, count) do
    {[_lparen, _keyword | module_tokens], remaining} = collect_sexpr(rest, [{:keyword, "assert_malformed"}, :lparen], 1)
    process_nested_module(module_tokens, dir, count, "fail")
    extract_modules(remaining, dir, count + 1)
  end

  defp extract_modules([:lparen, {:keyword, "assert_uninstantiable"} | rest], dir, count) do
    {[_lparen, _keyword | module_tokens], remaining} = collect_sexpr(rest, [{:keyword, "assert_uninstantiable"}, :lparen], 1)
    process_nested_module(module_tokens, dir, count, "fail")
    extract_modules(remaining, dir, count + 1)
  end

  defp extract_modules([:lparen, {:keyword, "module"} | rest], dir, count) do
    {module_tokens, remaining} = collect_sexpr(rest, [{:keyword, "module"}, :lparen], 1)

    case module_tokens do
      [:lparen, {:keyword, "module"}, {:keyword, "binary"} | _] ->
        extract_modules(remaining, dir, count)

      [:lparen, {:keyword, "module"}, {:keyword, "quote"} | _] ->
        extract_modules(remaining, dir, count)

      _ ->
        wat = tokens_to_wat(module_tokens)
        File.write!(Path.join([dir, "ok", "module_#{count}.wat"]), wat)
        extract_modules(remaining, dir, count + 1)
    end
  end

  defp extract_modules([_ | rest], dir, count), do: extract_modules(rest, dir, count)

  defp process_nested_module([:lparen, {:keyword, "module"} | _] = tokens, dir, count, sub) do
    {module_tokens, _} = collect_sexpr(Enum.drop(tokens, 2), [{:keyword, "module"}, :lparen], 1)
    case module_tokens do
        [:lparen, {:keyword, "module"}, {:keyword, "binary"} | _] -> :ok
        [:lparen, {:keyword, "module"}, {:keyword, "quote"} | _] -> :ok
        _ ->
            wat = tokens_to_wat(module_tokens)
            File.write!(Path.join([dir, sub, "module_#{count}.wat"]), wat)
    end
  end
  defp process_nested_module(_, _, _, _), do: :ok

  defp collect_sexpr(tokens, acc, 0), do: {Enum.reverse(acc), tokens}

  defp collect_sexpr([:lparen | rest], acc, depth),
    do: collect_sexpr(rest, [:lparen | acc], depth + 1)

  defp collect_sexpr([:rparen | rest], acc, depth),
    do: collect_sexpr(rest, [:rparen | acc], depth - 1)

  defp collect_sexpr([token | rest], acc, depth), do: collect_sexpr(rest, [token | acc], depth)
  defp collect_sexpr([], acc, _), do: {Enum.reverse(acc), []}

  defp tokens_to_wat(tokens) do
    tokens
    |> Enum.map(fn
      :lparen -> "("
      :rparen -> ")"
      {:keyword, k} -> k
      {:id, id} -> "$#{id}"
      {:int, i} -> "#{i}"
      {:float, :infinity} -> "inf"
      {:float, :neg_infinity} -> "-inf"
      {:float, :nan} -> "nan"
      {:float, :neg_nan} -> "-nan"
      {:float, {:nan, _sign, payload}} -> "nan:#{payload}"
      {:float, f} -> "#{f}"
      {:string, s} -> "\"#{s}\""
      {:offset, o} -> "offset=#{o}"
      {:align, a} -> "align=#{a}"
    end)
    |> Enum.join(" ")
    |> String.replace("( ", "(")
    |> String.replace(" )", ")")
  end
end

[wast_path, output_dir] = System.argv()
SpecExtractor.run(wast_path, output_dir)
