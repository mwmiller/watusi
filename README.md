# Watusi

A native Elixir implementation for converting WebAssembly Text (WAT) to the WebAssembly Binary Format (WASM).

## Specifications

Watusi adheres to the following standards:

- WebAssembly Core Specification 1.0 (Binary and Text formats).
- WebAssembly Bulk Memory Operations Extension.
- WebAssembly Fixed-width SIMD Extension.
- WebAssembly Threads/Atomics Extension.
- WebAssembly Sign-extension Operators.
- WebAssembly Nontrapping Float-to-int Conversions.
- IEEE 754-2019 for floating-point representation.

## Features

- S-Expression and Flat Syntax: Supports both folded and flat WAT formats.
- Identifier Resolution: Resolves symbolic identifiers (e.g. $my_func, $my_var) to their numeric indices.
- Section Support: All core sections plus Custom (name) section.
- Instruction Support: Comprehensive support for Core 1.0, Bulk Memory, SIMD, and Atomics.
- Debug Names: Optional support for including identifiers in the binary via the 'name' section.
- LEB128 Encoding: Uses the leb128 package for efficient integer encoding.
- Verified Compatibility: Tested against the official wat2wasm tool.

## Testing

The test suite uses tools from the [WebAssembly Binary Toolkit (WABT)](https://github.com/WebAssembly/wabt) as a gold standard for correctness:

- `wat2wasm`: Used to verify bit-for-bit parity of the generated binary.
- `wasm-validate`: Used to ensure the generated binary adheres to the WebAssembly specification.

Additionally, several complex integration tests are sourced from [Eli Bendersky's wasm-wat-samples](https://github.com/eliben/wasm-wat-samples).

To run the tests, ensure these tools are available in your PATH.

## Usage

```elixir
wat = "(module (func (export \"main\") (result i32) i32.const 42))"
wasm = Watusi.to_wasm(wat)
# <<0, 97, 115, 109, 1, 0, 0, 0, ...>>

# Including debug names
wasm_with_names = Watusi.to_wasm(wat, debug_names: true)
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
