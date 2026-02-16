defmodule Watusi do
  @moduledoc """
  Watusi: A WebAssembly Text (WAT) to Binary (WASM) converter.
  """

  alias Watusi.Encoder
  alias Watusi.Lexer
  alias Watusi.Parser

  @doc """
  Converts WebAssembly Text (WAT) source to a WebAssembly Binary (WASM).

  Supports both string input and IOData.

  ## Options

    * `:debug_names` - If `true`, includes a 'name' custom section in the binary
      containing symbolic identifiers from the WAT source. Defaults to `false`.

  ## Examples

      iex> wat = "(module (func (export \\"main\\") (result i32) i32.const 42))"
      iex> wasm = Watusi.to_wasm(wat)
      iex> <<0, 97, 115, 109, 1, 0, 0, 0>> <> rest = wasm
      iex> byte_size(rest) > 0
      true

  """
  @spec to_wasm(iodata(), keyword()) :: binary()
  def to_wasm(source, opts \\ []) do
    source
    |> IO.iodata_to_binary()
    |> Lexer.tokenize()
    |> Parser.parse()
    |> Encoder.encode(opts)
  end
end
