defmodule Watusi.LEB128 do
  @moduledoc false
  # Fast LEB128 encoding for WebAssembly
  # Only implements encoding (not decoding) since that's all we need

  import Bitwise

  @doc "Encode unsigned integer as LEB128"
  def encode_unsigned(n) when n >= 0 and n < 128, do: <<n>>

  def encode_unsigned(n) when n >= 0 and n < 16384 do
    <<(n &&& 0x7F) ||| 0x80, n >>> 7>>
  end

  def encode_unsigned(n) when n >= 0 do
    encode_unsigned_loop(n, [])
  end

  defp encode_unsigned_loop(n, acc) when n < 128 do
    IO.iodata_to_binary([Enum.reverse(acc), n])
  end

  defp encode_unsigned_loop(n, acc) do
    byte = (n &&& 0x7F) ||| 0x80
    encode_unsigned_loop(n >>> 7, [byte | acc])
  end

  @doc "Encode signed integer as LEB128"
  def encode_signed(n) when n >= 0 and n < 64, do: <<n>>
  def encode_signed(n) when n >= -64 and n < 0, do: <<n &&& 0x7F>>

  def encode_signed(n) when n >= 64 and n < 8192 do
    <<(n &&& 0x7F) ||| 0x80, n >>> 7 &&& 0x7F>>
  end

  def encode_signed(n) when n >= -8192 and n < -64 do
    <<(n &&& 0x7F) ||| 0x80, n >>> 7 &&& 0x7F>>
  end

  def encode_signed(n) do
    encode_signed_loop(n, [])
  end

  defp encode_signed_loop(n, acc) do
    byte = n &&& 0x7F
    n = n >>> 7

    # Check if we're done: for positive numbers, n should be 0 and high bit clear
    # for negative numbers, n should be -1 and high bit set
    done? =
      (n == 0 and (byte &&& 0x40) == 0) or
        (n == -1 and (byte &&& 0x40) != 0)

    case done? do
      true -> IO.iodata_to_binary([Enum.reverse(acc), byte])
      false -> encode_signed_loop(n, [byte ||| 0x80 | acc])
    end
  end
end
