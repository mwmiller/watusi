# Watusi

A native Elixir implementation for converting WebAssembly Text (WAT) to the WebAssembly Binary Format (WASM).

## Specifications

Watusi adheres to the following standards:

- WebAssembly Core Specification 1.0 (Binary and Text formats).
- WebAssembly Bulk Memory Operations Extension.
- IEEE 754-2019 for floating-point representation.

## Features

- S-Expression and Flat Syntax: Supports both folded and flat WAT formats.
- Identifier Resolution: Resolves symbolic identifiers (e.g. $my_func, $my_var) to their numeric indices.
- Section Support: Type, Import, Function, Table, Memory, Global, Export, Start, Element, Code, Data, and Data Count sections.
- Bulk Memory Operations: Support for memory.init, data.drop, memory.copy, memory.fill, table.init, elem.drop, and table.copy.
- LEB128 Encoding: Uses the leb128 package for efficient integer encoding.
- Verified Compatibility: Tested against the official wat2wasm tool.

## Testing

The test suite uses `wat2wasm` from the [WebAssembly Binary Toolkit (WABT)](https://github.com/WebAssembly/wabt) as a gold standard for comparison. To run the tests, ensure `wat2wasm` is available in your PATH.

## Usage

```elixir
wat = "(module (func (export \"main\") (result i32) i32.const 42))"
wasm = Watusi.to_wasm(wat)
# <<0, 97, 115, 109, 1, 0, 0, 0, ...>>
```

## Installation

The package can be installed by adding `watusi` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:watusi, "~> 0.1.0"}
  ]
end
```
