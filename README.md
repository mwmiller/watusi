# Watusi

A native Elixir implementation for converting WebAssembly Text (WAT) to the WebAssembly Binary Format (WASM).

Watusi provides a pure-Elixir pipeline for transforming human-readable WebAssembly into the standard binary format, supporting modern extensions and ensuring full compatibility with official tooling.

## Why Watusi?

- **Native Elixir**: No dependencies on external tools like `wat2wasm` for basic operation.
- **Modern Standards**: Support for Bulk Memory, SIMD, Threads, and Exception Handling.
- **Developer Friendly**: Optional support for debug names and detailed identifier resolution.
- **High Performance**: Optimized for speed and low memory overhead during the encoding process.

## Specifications

Watusi adheres to the following standards:

- WebAssembly Core Specification 1.0 (Binary and Text formats).
- WebAssembly Bulk Memory Operations Extension.
- WebAssembly Fixed-width SIMD Extension.
- WebAssembly Threads/Atomics Extension.
- WebAssembly Sign-extension Operators.
- WebAssembly Nontrapping Float-to-int Conversions.
- WebAssembly Exception Handling Proposal.
- IEEE 754-2019 for floating-point representation.

## Installation

Add `watusi` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:watusi, "~> 0.1.0"}
  ]
end
```

## Usage

The primary entry point is `Watusi.to_wasm/2`. It accepts WAT source as a string or iodata and returns the compiled WASM binary.

```elixir
wat = \"\"\"
(module
  (func (export \"add\") (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add)
)
\"\"\"

wasm = Watusi.to_wasm(wat)
# <<0, 97, 115, 109, 1, 0, 0, 0, ...>>
```

### Debug Names

You can include symbolic identifiers in the binary by passing `debug_names: true`. This adds a standard `name` custom section to the output.

```elixir
wasm_with_names = Watusi.to_wasm(wat, debug_names: true)
```

## Testing

Watusi is rigorously tested against the [WebAssembly Binary Toolkit (WABT)](https://github.com/WebAssembly/wabt). Our test suite verifies bit-for-bit parity with `wat2wasm` and validates all generated output with `wasm-validate`.

Over 5,000 spec-compliant test vectors are included, covering core instructions and advanced extensions.

## License

Watusi is released under the MIT License. See the [LICENSE](LICENSE) file for details.
