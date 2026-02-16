defmodule Watusi.Encoder.Common do
  @moduledoc false

  def encode_u32(v), do: LEB128.encode_unsigned(v)

  def encode_vector(l, f), do: [encode_u32(length(l)), Enum.map(l, f)]

  def encode_vector(l, m, f), do: [encode_u32(length(l)), for(x <- l, do: f.(m.(x)))]

  def encode_string(s), do: [encode_u32(byte_size(s)), s]

  @empty_payloads [[], [<<0>>, []]]
  defguardp is_empty_payload(payload) when payload in @empty_payloads

  def encode_section(_id, payload) when is_empty_payload(payload), do: []
  def encode_section(id, p), do: [id, encode_u32(IO.iodata_length(p)), p]
end
