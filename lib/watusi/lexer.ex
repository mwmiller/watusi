defmodule Watusi.Lexer do
  @moduledoc """
  Lexer for WebAssembly Text format (WAT).
  """

  @id_chars ~c"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'*+-./:<=>?@\\^_`|~"

  # Guard to check if a character is valid for a WAT identifier.
  defguardp is_id_char(c) when c in @id_chars

  # Guard to check if a character can start a WAT atom (keyword or instruction).
  defguardp is_atom_start(c) when c in ?a..?z or c in ?0..?9 or c in [?., ?_, ?-]

  def tokenize(input) do
    do_tokenize(input, [])
  end

  defp do_tokenize(<<>>, acc), do: Enum.reverse(acc)

  # Comments
  defp do_tokenize(<<";;", rest::binary>>, acc) do
    rest |> skip_line() |> do_tokenize(acc)
  end

  defp do_tokenize(<<"(;", rest::binary>>, acc) do
    rest |> skip_block_comment(1) |> do_tokenize(acc)
  end

  # Whitespace
  defp do_tokenize(<<c, rest::binary>>, acc) when c in [?\s, ?\t, ?\n, ?\r] do
    do_tokenize(rest, acc)
  end

  # Delimiters
  defp do_tokenize(<<"(", rest::binary>>, acc), do: do_tokenize(rest, [:lparen | acc])
  defp do_tokenize(<<")", rest::binary>>, acc), do: do_tokenize(rest, [:rparen | acc])

  # Strings
  defp do_tokenize(<<"\"", rest::binary>>, acc) do
    {string, rest} = read_string(rest, <<>>)
    do_tokenize(rest, [{:string, string} | acc])
  end

  # Identifiers
  defp do_tokenize(<<"$", rest::binary>>, acc) do
    {id, rest} = read_identifier(rest, <<>>)
    do_tokenize(rest, [{:id, id} | acc])
  end

  # Atoms (keywords, instructions, or numbers)
  defp do_tokenize(<<c, rest::binary>>, acc) when is_atom_start(c) do
    {atom, rest} = read_atom(<<c>>, rest)
    do_tokenize(rest, [classify_atom(atom) | acc])
  end

  defp do_tokenize(<<c, _rest::binary>>, _acc) do
    raise "Unexpected character: #{<<c>>}"
  end

  defp skip_line(<<?\n, rest::binary>>), do: rest
  defp skip_line(<<_, rest::binary>>), do: skip_line(rest)
  defp skip_line(<<>>), do: <<>>

  defp skip_block_comment(<<";)", rest::binary>>, 1), do: rest
  defp skip_block_comment(<<";)", rest::binary>>, depth), do: skip_block_comment(rest, depth - 1)
  defp skip_block_comment(<<"(;", rest::binary>>, depth), do: skip_block_comment(rest, depth + 1)
  defp skip_block_comment(<<_, rest::binary>>, depth), do: skip_block_comment(rest, depth)
  defp skip_block_comment(<<>>, _depth), do: <<>>

  defp read_string(<<"\"", rest::binary>>, acc), do: {acc, rest}

  defp read_string(<<"\\", c, rest::binary>>, acc),
    do: read_string(rest, <<acc::binary, escape(c)>>)

  defp read_string(<<c, rest::binary>>, acc), do: read_string(rest, <<acc::binary, c>>)

  defp escape(?n), do: ?\n
  defp escape(?r), do: ?\r
  defp escape(?t), do: ?\t
  defp escape(?\\), do: ?\\
  defp escape(?"), do: ?"
  defp escape(c), do: c

  defp read_identifier(<<c, rest::binary>>, acc) when is_id_char(c) do
    read_identifier(rest, <<acc::binary, c>>)
  end

  defp read_identifier(rest, acc), do: {acc, rest}

  defp read_atom(acc, <<c, rest::binary>>) when is_id_char(c) do
    read_atom(<<acc::binary, c>>, rest)
  end

  defp read_atom(acc, rest), do: {acc, rest}

  defp classify_atom(atom) do
    cond do
      hex_float_string?(atom) -> {:float, parse_hex_float(atom)}
      integer_string?(atom) -> {:int, parse_integer(atom)}
      float_string?(atom) -> {:float, parse_float(atom)}
      atom in ["inf", "-inf", "nan"] -> {:float, parse_special_float(atom)}
      String.starts_with?(atom, "offset=") -> {:offset, parse_kv_int(atom, "offset=")}
      String.starts_with?(atom, "align=") -> {:align, parse_kv_int(atom, "align=")}
      true -> {:keyword, atom}
    end
  end

  defp parse_kv_int(atom, prefix) do
    atom |> String.replace(prefix, "") |> parse_integer()
  end

  defp integer_string?(s) do
    case s do
      "0x" <> hex -> match?({_, ""}, Integer.parse(hex, 16))
      _ -> match?({_, ""}, Integer.parse(s))
    end
  end

  defp parse_integer(s) do
    case s do
      "0x" <> hex -> String.to_integer(hex, 16)
      _ -> String.to_integer(s)
    end
  end

  defp float_string?(s) do
    match?({_, ""}, Float.parse(s))
  end

  defp parse_float(s) do
    {f, ""} = Float.parse(s)
    f
  end

  defp hex_float_string?(s) do
    s =~ ~r/^[+-]?0x[0-9a-fA-F]*(\.[0-9a-fA-F]*)?[pP][+-]?[0-9]+$/
  end

  defp parse_hex_float(s) do
    # Format: [+-]?0x<significand>p<exponent>
    {sign, rest} =
      case s do
        "-" <> tail -> {-1, tail}
        "+" <> tail -> {1, tail}
        _ -> {1, s}
      end

    "0x" <> rest = rest
    [significand_str, exponent_str] = String.split(rest, ~r/[pP]/)

    significand =
      case String.split(significand_str, ".") do
        [whole] ->
          String.to_integer(whole, 16) / 1

        [whole, frac] ->
          w =
            case whole do
              "" -> 0
              _ -> String.to_integer(whole, 16)
            end

          f = parse_hex_frac(frac)
          w + f
      end

    exponent = String.to_integer(exponent_str)
    sign * significand * :math.pow(2, exponent)
  end

  defp parse_hex_frac(frac) do
    frac
    |> String.codepoints()
    |> Enum.with_index(1)
    |> Enum.reduce(0.0, fn {char, idx}, acc ->
      val = String.to_integer(char, 16)
      acc + val / :math.pow(16, idx)
    end)
  end

  defp parse_special_float("inf"), do: :infinity
  defp parse_special_float("-inf"), do: :neg_infinity
  defp parse_special_float("nan"), do: :nan
end
