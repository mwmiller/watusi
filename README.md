# Watusi

A high-performance, native Elixir implementation for converting WebAssembly Text (WAT) to the WebAssembly Binary Format (WASM).

Watusi provides a pure-Elixir pipeline for transforming human-readable WebAssembly into the standard binary format, supporting modern extensions and ensuring full compatibility with official tooling.

## Why Watusi?

- **Native Elixir**: No external dependencies or tools required - pure Elixir implementation.
- **High Performance**: Optimized lexer and encoder with compile-time code generation for maximum speed.
- **Modern Standards**: Core 1.0 plus Bulk Memory, Fixed-width SIMD, Threads/Atomics, Sign-extension, Non-trapping float-to-int, and Exception Handling proposals. Reference-Types and Function-References are partially supported.
- **Developer Friendly**: Optional debug names and detailed identifier resolution.
- **Spec Compliant**: Tested against the official WABT spec vectors (5,000+) with bit-for-bit parity to `wat2wasm`.

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
- WebAssembly Reference-Types and Function-References (partial).
- IEEE 754-2019 for floating-point representation.

> The Garbage Collection proposal is not yet validated by the suite (see `test/spec_vectors/README.md`).

## Installation

Add `watusi` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:watusi, "~> 0.4.0"}
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

Watusi is tested against the [WebAssembly Binary Toolkit (WABT)](https://github.com/WebAssembly/wabt). The test suite compiles each WABT `.wat` vector with `wat2wasm` and verifies bit-for-bit parity with Watusi's output, then validates generated binaries with `wasm-validate`.

Over 5,000 official spec vectors are included, covering core instructions and advanced extensions. A small subset (currently 75) that exercises features not yet fully supported—primarily Reference-Types and Function-References—is tagged `:known_failure` and excluded from the default run. Run them explicitly with `mix test --include known_failure`; see `test/spec_vectors/README.md` for details.

## License

Watusi is released under the MIT License. See the [LICENSE](LICENSE) file for details.
