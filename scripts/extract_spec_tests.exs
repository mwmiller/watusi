# Spec vector extractor.
#
# Splits a WebAssembly spec `.wast` file into one `.wat` module per test
# vector, writing them under <output_dir>/ok/module_N.wat (valid modules)
# and <output_dir>/fail/module_N.wat (modules wrapped in assert_invalid /
# assert_malformed / assert_uninstantiable).
#
# Translation rules (kept in sync with upstream wast semantics):
#   * Every top-level `(module ...)` becomes an `ok/` vector, except
#     `(module binary ...)` and `(module quote ...)` which encode raw bytes /
#     text and are skipped (Watusi is a text encoder, not a decoder).
#   * The `(module ...)` nested inside `assert_invalid`, `assert_malformed`
#     and `assert_uninstantiable` becomes a `fail/` vector. `assert_malformed`
#     is a syntax-level failure, so some of its nested modules are
#     `(module binary ...)` and are skipped. `assert_uninstantiable` wraps a
#     module that is well-formed but fails at instantiation; such modules are
#     often still accepted by wasm-tools `validate`, so the harness checks them
#     for byte-parity against the wasm-tools reference instead of requiring a
#     refusal (see test/test_helper.exs).
#   * Everything else (assert_return, assert_trap, assert_exhaustion, ...) is
#     an execution-time check on a module that was already emitted as an ok
#     vector, and is dropped.
#
# Unlike the original extractor this walks the raw text rather than round-
# tripping through the Watusi lexer, so upstream modules are preserved
# verbatim (comments, formatting and all) and newer upstream syntax cannot
# break extraction.
#
# Usage: elixir scripts/extract_spec_tests.exs <in.wast> <out_dir>

defmodule SpecExtractor do
  def run(wast_path, output_dir) do
    File.mkdir_p!(Path.join(output_dir, "ok"))
    File.mkdir_p!(Path.join(output_dir, "fail"))
    text = File.read!(wast_path)

    {ok, fail} =
      text
      |> top_level_forms()
      |> Enum.reduce({0, 0}, fn form, {ok, fail} ->
        case classify(form) do
          {:ok, wat} ->
            File.write!(Path.join([output_dir, "ok", "module_#{ok}.wat"]), normalize_wat(wat))
            {ok + 1, fail}

          {:fail, wat} ->
            File.write!(Path.join([output_dir, "fail", "module_#{fail}.wat"]), normalize_wat(wat))
            {ok, fail + 1}

          :skip ->
            {ok, fail}
        end
      end)

    IO.puts("#{Path.basename(wast_path)}: #{ok} ok, #{fail} fail")
  end

  defp normalize_wat(wat), do: String.trim_trailing(wat) <> "\n"

  # --- top-level form splitting ------------------------------------------

  # Splits a wast into top-level, paren-balanced forms. Strings, escapes,
  # line comments and (possibly nested) block comments are tracked so parens
  # inside them are ignored.
  defp top_level_forms(text) do
    text
    |> String.graphemes()
    |> split([], [], 0, 0, false, false)
    |> Enum.reverse()
  end

  # buf (reversed), forms, depth, block_depth, in_string, in_line
  defp split([], buf, forms, _depth, _block, _in_string, _in_line),
    do: [finish(buf) | forms]

  defp split([g | rest], buf, forms, depth, block, in_string, in_line) do
    cond do
      in_line ->
        if g == "\n" do
          split(rest, [g | buf], forms, depth, block, in_string, false)
        else
          split(rest, [g | buf], forms, depth, block, in_string, in_line)
        end

      block > 0 ->
        split_in_block(g, rest, buf, forms, depth, block)

      in_string ->
        if g == "\\" do
          case rest do
            [esc | rest2] -> split(rest2, [esc, g | buf], forms, depth, block, in_string, in_line)
            [] -> split([], [g | buf], forms, depth, block, in_string, in_line)
          end
        else
          in_string = if g == "\"", do: false, else: in_string
          split(rest, [g | buf], forms, depth, block, in_string, in_line)
        end

      true ->
        split_toplevel(g, rest, buf, forms, depth, block)
    end
  end

  # inside a (; ... ;) block comment: only "(;" and ";)" change nesting
  defp split_in_block("(", [";" | rest], buf, forms, depth, block),
    do: split(rest, [";", "(" | buf], forms, depth, block + 1, false, false)

  defp split_in_block(";", [")" | rest], buf, forms, depth, block),
    do: split(rest, [")", ";" | buf], forms, depth, block - 1, false, false)

  defp split_in_block(g, rest, buf, forms, depth, block),
    do: split(rest, [g | buf], forms, depth, block, false, false)

  defp split_toplevel("\"", rest, buf, forms, depth, block),
    do: split(rest, ["\"" | buf], forms, depth, block, true, false)

  defp split_toplevel(";", [";" | rest], buf, forms, depth, block),
    do: split(rest, [";", ";" | buf], forms, depth, block, false, true)

  defp split_toplevel("(", [";" | rest], buf, forms, depth, block),
    do: split(rest, [";", "(" | buf], forms, depth, block + 1, false, false)

  defp split_toplevel("(", rest, buf, forms, depth, block),
    do: split(rest, ["(" | buf], forms, depth + 1, block, false, false)

  defp split_toplevel(")", rest, buf, forms, depth, block) do
    case depth do
      1 ->
        # closing paren brings us back to depth 0: flush the form
        form = finish([")" | buf])
        split(rest, [], [form | forms], 0, block, false, false)

      _ ->
        split(rest, [")" | buf], forms, depth - 1, block, false, false)
    end
  end

  defp split_toplevel(g, rest, buf, forms, depth, block),
    do: split(rest, [g | buf], forms, depth, block, false, false)

  defp finish(buf), do: buf |> Enum.reverse() |> Enum.join()

  # --- classification ----------------------------------------------------

  defp classify(form) do
    trimmed = strip_leading_junk(form) |> String.trim()

    case keyword(trimmed) do
      "module" ->
        if binary_or_quote?(trimmed), do: :skip, else: {:ok, ensure_newline(trimmed)}

      kw when kw in ["assert_invalid", "assert_malformed", "assert_uninstantiable"] ->
        case nested_module(trimmed) do
          nil -> :skip
          wat -> {:fail, ensure_newline(wat)}
        end

      _ ->
        :skip
    end
  end

  # First keyword of a form, e.g. "module", "assert_invalid", "register".
  # Comments/whitespace may appear between "(" and the keyword, and an
  # empty "(module)" reports as "module".
  defp keyword(trimmed) do
    inner =
      trimmed
      |> String.slice(1..-1//1)
      |> strip_leading_junk()
      |> String.trim_leading()

    cond do
      String.starts_with?(inner, ")") -> "module"
      true -> case Regex.run(~r/^[a-zA-Z_][a-zA-Z0-9_]*/, inner) do
        [kw] -> kw
        nil -> nil
      end
    end
  end

  defp binary_or_quote?(trimmed) do
    rest =
      trimmed
      |> String.slice(1..-1//1)
      |> strip_leading_junk()
      |> String.trim_leading()
      |> String.replace_prefix("module", "")
      |> strip_leading_junk()
      |> String.trim_leading()

    String.starts_with?(rest, "binary") or String.starts_with?(rest, "quote")
  end

  # Buffered forms carry any leading whitespace/comments that appeared before
  # them; drop those so the form starts at its own opening paren.
  defp strip_leading_junk(s) do
    s = String.trim_leading(s)

    cond do
      String.starts_with?(s, ";;") ->
        case :binary.match(s, "\n") do
          {i, 1} -> strip_leading_junk(String.slice(s, (i + 1)..-1//1))
          :nomatch -> ""
        end

      String.starts_with?(s, "(;") ->
        case s |> String.graphemes() |> skip_block_comment() do
          {:ok, rest} -> strip_leading_junk(Enum.join(rest))
          :error -> ""
        end

      true ->
        s
    end
  end

  # Skips a leading (; ... ;) block comment, honouring nesting. The opening
  # "(;" is already consumed by the caller, so scanning starts at depth 1.
  defp skip_block_comment(graphemes), do: do_skip_block(Enum.drop(graphemes, 2), 1)

  defp do_skip_block([], _depth), do: :error
  defp do_skip_block(["(", ";" | rest], depth), do: do_skip_block(rest, depth + 1)

  defp do_skip_block([";", ")" | rest], 1), do: {:ok, rest}
  defp do_skip_block([";", ")" | rest], depth), do: do_skip_block(rest, depth - 1)
  defp do_skip_block([_ | rest], depth), do: do_skip_block(rest, depth)

  # Extracts the (module ...) nested directly inside an assert_* form.
  defp nested_module(trimmed) do
    inner =
      trimmed
      |> String.slice(1..-1//1)
      |> strip_leading_junk()
      |> String.trim_leading()

    rest =
      case Regex.run(~r/^assert_(?:invalid|malformed|uninstantiable)\s*/, inner) do
        [match] -> inner |> String.replace_prefix(match, "") |> strip_leading_junk() |> String.trim_leading()
        nil -> inner
      end

    case first_module(rest) do
      {:ok, wat} -> if binary_or_quote?(wat), do: nil, else: wat
      _ -> nil
    end
  end

  # Scans for the first paren-balanced form in a string and returns its text.
  # Reuses the string/comment-aware top-level splitter so parens inside
  # strings and comments are ignored.
  defp first_module(text) do
    case top_level_forms(text) do
      [first | _] -> {:ok, String.trim(first)}
      [] -> :error
    end
  end

  defp ensure_newline(s), do: if(String.ends_with?(s, "\n"), do: s, else: s <> "\n")
end

[wast_path, output_dir] = System.argv()
SpecExtractor.run(wast_path, output_dir)
