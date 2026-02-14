defmodule Watusi do
  @moduledoc """
  Watusi: A WebAssembly Text (WAT) to Binary (WASM) converter.
  """

  alias Watusi.Encoder
  alias Watusi.Lexer
  alias Watusi.Parser

  @doc """
  Converts WAT source (string or IOData) to WASM binary.
  """
  def to_wasm(source, opts \\ []) do
    source
    |> IO.iodata_to_binary()
    |> Lexer.tokenize()
    |> Parser.parse()
    |> Encoder.encode(opts)
  end
end
