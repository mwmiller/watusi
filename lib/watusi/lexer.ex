defmodule Watusi.Lexer do
  @moduledoc false

  # Valid characters for identifiers as per spec, expanded for WAST/annotations
  @id_chars ~c"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'*+-./:<=>?@\\^_`|~,;[]{}#`"

  @special_floats %{
    "inf" => :infinity,
    "+inf" => :infinity,
    "-inf" => :neg_infinity,
    "nan" => :nan,
    "+nan" => :nan,
    "-nan" => :neg_nan
  }

  defguardp is_id_char(c) when c in @id_chars

  defguardp is_hex(c) when c in ?0..?9 or c in ?a..?f or c in ?A..?F

  def tokenize(input), do: do_tokenize(input, [])

  defp do_tokenize(<<>>, acc), do: Enum.reverse(acc)

  # Standard line comments
  defp do_tokenize(<<";;", rest::binary>>, acc) do
    rest |> skip_line() |> do_tokenize(acc)
  end

  # Nestable block comments
  defp do_tokenize(<<"(;", rest::binary>>, acc) do
    rest |> skip_block_comment(1) |> do_tokenize(acc)
  end

  # Standard whitespace separation
  defp do_tokenize(<<c, rest::binary>>, acc) when c in [?\s, ?\t, ?\n, ?\r] do
    do_tokenize(rest, acc)
  end

  defp do_tokenize(<<"(", rest::binary>>, acc), do: do_tokenize(rest, [:lparen | acc])
  defp do_tokenize(<<")", rest::binary>>, acc), do: do_tokenize(rest, [:rparen | acc])

  defp do_tokenize(<<"\"", rest::binary>>, acc) do
    {string, rest} = read_string(rest, <<>>)
    do_tokenize(rest, [{:string, string} | acc])
  end

  # Symbolic identifiers
  defp do_tokenize(<<"$", rest::binary>>, acc) do
    {id, rest} = read_identifier(rest, <<>>)
    do_tokenize(rest, [{:id, id} | acc])
  end

  defp do_tokenize(<<c, rest::binary>>, acc) when is_id_char(c) do
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

  # Hex-encoded unicode escapes
  defp read_string(<<"\\u{", rest::binary>>, acc) do
    {hex, <<"}", rest::binary>>} = read_hex_until_brace(rest, <<>>)
    codepoint = String.to_integer(hex, 16)
    utf8 = <<codepoint::utf8>>
    read_string(rest, <<acc::binary, utf8::binary>>)
  end

  # Literal byte escapes
  defp read_string(<<"\\", a, b, rest::binary>>, acc) when is_hex(a) and is_hex(b) do
    byte = String.to_integer(<<a, b>>, 16)
    read_string(rest, <<acc::binary, byte>>)
  end

  defp read_string(<<"\\", c, rest::binary>>, acc),
    do: read_string(rest, <<acc::binary, escape(c)>>)

  defp read_string(<<c, rest::binary>>, acc), do: read_string(rest, <<acc::binary, c>>)

  defp read_hex_until_brace(<<c, rest::binary>>, acc) when is_hex(c),
    do: read_hex_until_brace(rest, <<acc::binary, c>>)

  defp read_hex_until_brace(rest, acc), do: {acc, rest}

  defp escape(?n), do: ?\n
  defp escape(?r), do: ?\r
  defp escape(?t), do: ?\t
  defp escape(?\\), do: ?\\
  defp escape(?"), do: ?"
  defp escape(c), do: c

  defp read_identifier(<<c, rest::binary>>, acc) when is_id_char(c),
    do: read_identifier(rest, <<acc::binary, c>>)

  defp read_identifier(rest, acc), do: {acc, rest}

  defp read_atom(acc, <<c, rest::binary>>) when is_id_char(c),
    do: read_atom(<<acc::binary, c>>, rest)

  defp read_atom(acc, rest), do: {acc, rest}

  defp classify_atom(atom) do
    case Map.get(@special_floats, atom) do
      nil -> do_classify_atom(atom)
      val -> {:float, val}
    end
  end

  defp do_classify_atom(atom) do
    # Memory and table instructions often use kv-pair immediates
    case atom do
      <<"offset=", val::binary>> -> {:offset, parse_integer(val)}
      <<"align=", val::binary>> -> {:align, parse_integer(val)}
      other -> determine_numeric_type(other)
    end
  end

  defp determine_numeric_type(atom) do
    # We check for more specific/constrained numeric formats first
    cond do
      hex_float_string?(atom) -> {:float, parse_hex_float(atom)}
      nan_payload_string?(atom) -> {:float, parse_nan_payload(atom)}
      integer_string?(atom) -> {:int, parse_integer(atom)}
      float_string?(atom) -> {:float, parse_float(atom), atom}
      true -> {:keyword, atom}
    end
  end

  defp integer_string?(s) do
    s = String.replace(s, "_", "")

    case s do
      <<"0x", hex::binary>> -> hex_string?(hex)
      <<"-0x", hex::binary>> -> hex_string?(hex)
      <<"+0x", hex::binary>> -> hex_string?(hex)
      _ -> match?({_, ""}, Integer.parse(s))
    end
  end

  defp hex_string?(""), do: false
  defp hex_string?(s), do: do_hex_string?(s)

  defp do_hex_string?(<<c, rest::binary>>) when is_hex(c), do: do_hex_string?(rest)
  defp do_hex_string?(<<>>), do: true
  defp do_hex_string?(_), do: false

  defp parse_integer(s) do
    s = String.replace(s, "_", "")

    case s do
      <<"0x", hex::binary>> -> String.to_integer(hex, 16)
      <<"-0x", hex::binary>> -> -String.to_integer(hex, 16)
      <<"+0x", hex::binary>> -> String.to_integer(hex, 16)
      <<"+", rest::binary>> -> String.to_integer(rest)
      _ -> String.to_integer(s)
    end
  end

  defp float_string?(s) do
    clean_s = String.replace(s, "_", "")

    case Float.parse(clean_s) do
      {_, ""} ->
        true

      {_, rem} ->
        case rem do
          "." ->
            match?({_, ""}, Float.parse(clean_s <> "0"))

          _ ->
            (String.starts_with?(rem, ".e") or String.starts_with?(rem, ".E")) and
              match?(
                {_, ""},
                Float.parse(
                  String.replace(clean_s, ".e", "e", global: false)
                  |> String.replace(".E", "E", global: false)
                )
              )
        end

      _ ->
        false
    end
  end

  defp parse_float(s) do
    s = String.replace(s, "_", "")

    case Float.parse(s) do
      {f, ""} -> f
      {_, rem} -> handle_float_remainder(s, rem)
      :error -> handle_float_error(s)
    end
  end

  defp handle_float_remainder(s, "."), do: elem(Float.parse(s <> "0"), 0)

  defp handle_float_remainder(s, rem) do
    case String.starts_with?(rem, ".e") or String.starts_with?(rem, ".E") do
      true ->
        s_fixed =
          String.replace(s, ".e", "e", global: false)
          |> String.replace(".E", "E", global: false)

        elem(Float.parse(s_fixed), 0)

      false ->
        case s do
          <<"+", rest::binary>> -> elem(Float.parse(rest), 0)
          _ -> elem(Float.parse(s), 0)
        end
    end
  end

  defp handle_float_error(<<"+", rest::binary>>), do: parse_float(rest)
  defp handle_float_error(s), do: raise("Invalid float literal: #{s}")

  defp hex_float_string?(s),
    do: s =~ ~r/^[+-]?0x([0-9a-fA-F_]+\.?|[0-9a-fA-F_]*\.[0-9a-fA-F_]*)([pP][+-]?[0-9_]+)?$/

  defp parse_hex_float(s) do
    # Hex floats are required for precise IEEE 754 representation.
    # We strip underscores which are purely for readability.
    s = String.replace(s, "_", "")

    {sign, rest} =
      case s do
        <<"-", tail::binary>> -> {-1, tail}
        <<"+", tail::binary>> -> {1, tail}
        _ -> {1, s}
      end

    <<"0x", rest::binary>> = rest

    {significand_str, exponent_str} =
      case String.split(rest, ~r/[pP]/, parts: 2) do
        [sig, exp] -> {sig, exp}
        [sig] -> {sig, "0"}
      end

    significand = parse_hex_significand(significand_str)
    exponent = String.to_integer(exponent_str)

    try do
      sign * significand * :math.pow(2.0, exponent)
    rescue
      ArithmeticError ->
        case sign > 0 do
          true -> :infinity
          false -> :neg_infinity
        end
    end
  end

  defp parse_hex_significand(s) do
    case String.split(s, ".") do
      [whole] ->
        String.to_integer(whole, 16) / 1

      [whole, ""] ->
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
  end

  defp parse_hex_frac(frac), do: do_parse_hex_frac(frac, 16.0, 0.0)

  defp do_parse_hex_frac(<<c, rest::binary>>, divisor, acc) do
    val = String.to_integer(<<c>>, 16)
    do_parse_hex_frac(rest, divisor * 16.0, acc + val / divisor)
  end

  defp do_parse_hex_frac(<<>>, _divisor, acc), do: acc

  defp nan_payload_string?(s),
    do: s =~ ~r/^[+-]?nan:0x[0-9a-fA-F_]+$/ or s =~ ~r/^[+-]?nan:[0-9_]+$/

  defp parse_nan_payload(s) do
    # NaN payloads allow encoding diagnostic information in the float bit-pattern
    s = String.replace(s, "_", "")

    {sign, rest} =
      case s do
        <<"-", tail::binary>> -> {-1, tail}
        <<"+", tail::binary>> -> {1, tail}
        _ -> {1, s}
      end

    <<"nan:", payload_str::binary>> = rest

    payload =
      case payload_str do
        <<"0x", hex::binary>> -> String.to_integer(hex, 16)
        _dec -> String.to_integer(payload_str)
      end

    {:nan, sign, payload}
  end
end
