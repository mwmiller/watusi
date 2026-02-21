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
    {id, rest} = read_identifier(rest, nil)
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

  defp read_identifier(input, _acc) do
    len = count_id_chars(input, 0)
    <<id::binary-size(len), rest::binary>> = input
    {id, rest}
  end

  defp read_atom(first, rest) do
    len = count_id_chars(rest, 0)
    <<atom_tail::binary-size(len), remaining::binary>> = rest
    {<<first::binary, atom_tail::binary>>, remaining}
  end

  defp count_id_chars(<<c, rest::binary>>, count) when is_id_char(c),
    do: count_id_chars(rest, count + 1)

  defp count_id_chars(_, count), do: count

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
    case s do
      <<"0x", hex::binary>> -> hex_string?(hex)
      <<"-0x", hex::binary>> -> hex_string?(hex)
      <<"+0x", hex::binary>> -> hex_string?(hex)
      _ -> decimal_string?(s)
    end
  end

  defp decimal_string?(<<c, rest::binary>>) when c in ?0..?9, do: decimal_string?(rest)
  defp decimal_string?(<<"-", rest::binary>>), do: decimal_string?(rest)
  defp decimal_string?(<<"+", rest::binary>>), do: decimal_string?(rest)
  defp decimal_string?(<<"_", rest::binary>>), do: decimal_string?(rest)
  defp decimal_string?(<<>>), do: true
  defp decimal_string?(_), do: false

  defp hex_string?(""), do: false
  defp hex_string?(s), do: do_hex_string?(s)

  defp do_hex_string?(<<c, rest::binary>>) when is_hex(c), do: do_hex_string?(rest)
  defp do_hex_string?(<<"_", rest::binary>>), do: do_hex_string?(rest)
  defp do_hex_string?(<<>>), do: true
  defp do_hex_string?(_), do: false

  defp parse_integer(s) do
    case s do
      <<"0x", hex::binary>> -> parse_hex_int(hex, 1)
      <<"-0x", hex::binary>> -> parse_hex_int(hex, -1)
      <<"+0x", hex::binary>> -> parse_hex_int(hex, 1)
      <<"+", rest::binary>> -> parse_decimal_int(rest)
      _ -> parse_decimal_int(s)
    end
  end

  defp parse_hex_int(s, sign), do: sign * parse_hex_int_acc(s, 0)

  defp parse_hex_int_acc(<<c, rest::binary>>, acc) when c in ?0..?9,
    do: parse_hex_int_acc(rest, acc * 16 + (c - ?0))

  defp parse_hex_int_acc(<<c, rest::binary>>, acc) when c in ?a..?f,
    do: parse_hex_int_acc(rest, acc * 16 + (c - ?a + 10))

  defp parse_hex_int_acc(<<c, rest::binary>>, acc) when c in ?A..?F,
    do: parse_hex_int_acc(rest, acc * 16 + (c - ?A + 10))

  defp parse_hex_int_acc(<<"_", rest::binary>>, acc), do: parse_hex_int_acc(rest, acc)
  defp parse_hex_int_acc(<<>>, acc), do: acc

  defp parse_decimal_int(s), do: parse_decimal_int_acc(s, 0, 1)

  defp parse_decimal_int_acc(<<"-", rest::binary>>, 0, 1),
    do: parse_decimal_int_acc(rest, 0, -1)

  defp parse_decimal_int_acc(<<c, rest::binary>>, acc, sign) when c in ?0..?9,
    do: parse_decimal_int_acc(rest, acc * 10 + (c - ?0), sign)

  defp parse_decimal_int_acc(<<"_", rest::binary>>, acc, sign),
    do: parse_decimal_int_acc(rest, acc, sign)

  defp parse_decimal_int_acc(<<>>, acc, sign), do: acc * sign

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

  defp hex_float_string?(<<"0x", rest::binary>>), do: hex_float_body?(rest)
  defp hex_float_string?(<<"+0x", rest::binary>>), do: hex_float_body?(rest)
  defp hex_float_string?(<<"-0x", rest::binary>>), do: hex_float_body?(rest)
  defp hex_float_string?(_), do: false

  defp hex_float_body?(<<c, rest::binary>>) when is_hex(c), do: hex_float_body?(rest)
  defp hex_float_body?(<<"_", rest::binary>>), do: hex_float_body?(rest)
  defp hex_float_body?(<<".", rest::binary>>), do: hex_float_after_dot?(rest)
  defp hex_float_body?(<<"p", rest::binary>>), do: hex_float_exp?(rest)
  defp hex_float_body?(<<"P", rest::binary>>), do: hex_float_exp?(rest)
  defp hex_float_body?(<<>>), do: true
  defp hex_float_body?(_), do: false

  defp hex_float_after_dot?(<<c, rest::binary>>) when is_hex(c), do: hex_float_after_dot?(rest)
  defp hex_float_after_dot?(<<"_", rest::binary>>), do: hex_float_after_dot?(rest)
  defp hex_float_after_dot?(<<"p", rest::binary>>), do: hex_float_exp?(rest)
  defp hex_float_after_dot?(<<"P", rest::binary>>), do: hex_float_exp?(rest)
  defp hex_float_after_dot?(<<>>), do: true
  defp hex_float_after_dot?(_), do: false

  defp hex_float_exp?(<<"+", rest::binary>>), do: hex_float_exp_digits?(rest)
  defp hex_float_exp?(<<"-", rest::binary>>), do: hex_float_exp_digits?(rest)
  defp hex_float_exp?(rest), do: hex_float_exp_digits?(rest)

  defp hex_float_exp_digits?(<<c, rest::binary>>) when c in ?0..?9,
    do: hex_float_exp_digits?(rest)

  defp hex_float_exp_digits?(<<"_", rest::binary>>), do: hex_float_exp_digits?(rest)
  defp hex_float_exp_digits?(<<>>), do: true
  defp hex_float_exp_digits?(_), do: false

  defp parse_hex_float(s) do
    # Hex floats are required for precise IEEE 754 representation.
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
    exponent = parse_signed_decimal(exponent_str)

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

  defp parse_signed_decimal(s), do: parse_signed_decimal_acc(s, 0, 1)

  defp parse_signed_decimal_acc(<<"-", rest::binary>>, 0, 1),
    do: parse_signed_decimal_acc(rest, 0, -1)

  defp parse_signed_decimal_acc(<<"+", rest::binary>>, 0, 1),
    do: parse_signed_decimal_acc(rest, 0, 1)

  defp parse_signed_decimal_acc(<<c, rest::binary>>, acc, sign) when c in ?0..?9,
    do: parse_signed_decimal_acc(rest, acc * 10 + (c - ?0), sign)

  defp parse_signed_decimal_acc(<<"_", rest::binary>>, acc, sign),
    do: parse_signed_decimal_acc(rest, acc, sign)

  defp parse_signed_decimal_acc(<<>>, acc, sign), do: acc * sign

  defp parse_hex_significand(s) do
    case String.split(s, ".") do
      [whole] ->
        parse_hex_whole(whole) / 1

      [whole, ""] ->
        parse_hex_whole(whole) / 1

      [whole, frac] ->
        w = if whole == "", do: 0, else: parse_hex_whole(whole)
        f = parse_hex_frac(frac)
        w + f
    end
  end

  defp parse_hex_whole(s), do: parse_hex_whole_acc(s, 0)

  defp parse_hex_whole_acc(<<c, rest::binary>>, acc) when c in ?0..?9,
    do: parse_hex_whole_acc(rest, acc * 16 + (c - ?0))

  defp parse_hex_whole_acc(<<c, rest::binary>>, acc) when c in ?a..?f,
    do: parse_hex_whole_acc(rest, acc * 16 + (c - ?a + 10))

  defp parse_hex_whole_acc(<<c, rest::binary>>, acc) when c in ?A..?F,
    do: parse_hex_whole_acc(rest, acc * 16 + (c - ?A + 10))

  defp parse_hex_whole_acc(<<"_", rest::binary>>, acc), do: parse_hex_whole_acc(rest, acc)
  defp parse_hex_whole_acc(<<>>, acc), do: acc

  defp parse_hex_frac(frac), do: do_parse_hex_frac(frac, 16.0, 0.0)

  defp do_parse_hex_frac(<<c, rest::binary>>, divisor, acc) when c in ?0..?9 do
    do_parse_hex_frac(rest, divisor * 16.0, acc + (c - ?0) / divisor)
  end

  defp do_parse_hex_frac(<<c, rest::binary>>, divisor, acc) when c in ?a..?f do
    do_parse_hex_frac(rest, divisor * 16.0, acc + (c - ?a + 10) / divisor)
  end

  defp do_parse_hex_frac(<<c, rest::binary>>, divisor, acc) when c in ?A..?F do
    do_parse_hex_frac(rest, divisor * 16.0, acc + (c - ?A + 10) / divisor)
  end

  defp do_parse_hex_frac(<<"_", rest::binary>>, divisor, acc) do
    do_parse_hex_frac(rest, divisor, acc)
  end

  defp do_parse_hex_frac(<<>>, _divisor, acc), do: acc

  defp nan_payload_string?(<<"nan:0x", rest::binary>>), do: hex_string?(rest)
  defp nan_payload_string?(<<"+nan:0x", rest::binary>>), do: hex_string?(rest)
  defp nan_payload_string?(<<"-nan:0x", rest::binary>>), do: hex_string?(rest)
  defp nan_payload_string?(<<"nan:", rest::binary>>), do: decimal_string?(rest)
  defp nan_payload_string?(<<"+nan:", rest::binary>>), do: decimal_string?(rest)
  defp nan_payload_string?(<<"-nan:", rest::binary>>), do: decimal_string?(rest)
  defp nan_payload_string?(_), do: false

  defp parse_nan_payload(s) do
    # NaN payloads allow encoding diagnostic information in the float bit-pattern
    {sign, rest} =
      case s do
        <<"-", tail::binary>> -> {-1, tail}
        <<"+", tail::binary>> -> {1, tail}
        _ -> {1, s}
      end

    <<"nan:", payload_str::binary>> = rest

    payload =
      case payload_str do
        <<"0x", hex::binary>> -> parse_hex_whole(hex)
        _dec -> parse_decimal_int_acc(payload_str, 0, 1)
      end

    {:nan, sign, payload}
  end
end
