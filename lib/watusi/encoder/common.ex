defmodule Watusi.Encoder.Common do
  @moduledoc """
  Common encoding utilities for WASM.
  """

  def encode_u32(v), do: LEB128.encode_unsigned(v)

  def encode_vector(l, f), do: [encode_u32(length(l)), Enum.map(l, f)]

  def encode_vector(l, m, f), do: [encode_u32(length(l)), Enum.map(l, m) |> Enum.map(f)]

  def encode_string(s), do: [encode_u32(byte_size(s)), s]

  def encode_section(_id, payload) when payload in [[], [<<0>>, []]], do: []
  def encode_section(id, p), do: [id, encode_u32(IO.iodata_length(p)), p]
end
