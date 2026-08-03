# Watusi

A high-performance, native Elixir implementation for converting WebAssembly Text (WAT) to the WebAssembly Binary Format (WASM).

Watusi provides a pure-Elixir pipeline for transforming human-readable WebAssembly into the standard binary format, supporting modern extensions and ensuring full compatibility with official tooling.

## Why Watusi?

- **Native Elixir**: No external dependencies or tools required - pure Elixir implementation.
- **High Performance**: Optimized lexer and encoder with compile-time code generation for maximum speed.
- **Modern Standards**: Core 1.0 plus Bulk Memory, Fixed-width SIMD, Threads/Atomics, Sign-extension, Non-trapping float-to-int, Exception Handling, Garbage Collection, Reference-Types, and Function-References proposals.
- **Developer Friendly**: Optional debug names and detailed identifier resolution.
- **Spec Compliant**: Tested against the official spec vectors (5,000+) with bit-for-bit parity to `wasm-tools`.

## Performance

Watusi is optimized for speed with typical compilation times for real-world modules in the single-digit milliseconds range.

## Specifications

Watusi adheres to the following standards:

- WebAssembly Core Specification 1.0 (Binary and Text formats).
- WebAssembly Bulk Memory Operations Extension.
- WebAssembly Fixed-width SIMD Extension.
- WebAssembly Threads/Atomics Extension.
- WebAssembly Sign-extension Operators.
- WebAssembly Nontrapping Float-to-int Conversions.
- WebAssembly Exception Handling Proposal.
- WebAssembly Garbage Collection Proposal: recursive/sub-typed type groups, struct/array types, and GC/ref instructions.
- WebAssembly Reference-Types and Function-References.
- IEEE 754-2019 for floating-point representation.

## Installation

Add `watusi` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:watusi, "~> 0.6.0"}
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

### Binary Patching

For pre-compiled WASM templates, use `Watusi.Patcher` to replace data segments and global initializers:

```elixir
# Compile template once
template = Watusi.to_wasm(interpreter_wat)

# Patch with runtime data
wasm = Watusi.Patcher.patch(template,
  data: [{0x00000, story_bytes}],
  globals: %{0 => version}
)
```

See `doc/patcher.md` for details.

### Debug Names

You can include symbolic identifiers in the binary by passing `debug_names: true`. This adds a standard `name` custom section to the output.

```elixir
wasm_with_names = Watusi.to_wasm(wat, debug_names: true)
```

## Testing

Watusi is tested against the [bytecodealliance/wasm-tools](https://github.com/bytecodealliance/wasm-tools). The test suite compiles each `.wat` vector with `wasm-tools parse` (stripped with `wasm-tools strip --all`) and verifies bit-for-bit parity with Watusi's output, then validates generated binaries with `wasm-tools validate --features all`.

Over 5,000 official spec vectors are included, covering core instructions and advanced extensions, with the full suite at `5146/5146` passing against the pinned `wasm-tools` reference. See `test/spec_vectors/README.md` for details.

