# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.6.1] - 2026-08-03

### Added

- Quoted-string identifiers (`$"..."`) now lex as single identifier tokens, resolving identically to `$name`.
- Hex float literals with a trailing dot and no `p` exponent (`0x0123456789ABCDEF.`, `0xa0_ff.f141`) are supported.
- Nan payload lanes in `v128.const f32x4` / `v128.const f64x2` are encoded with their sign and payload bits preserved.

### Changed

- Spec vectors refreshed from a pinned upstream `WebAssembly/spec` commit (`bdd7164`, 2026-07-28) via a rewritten `scripts/extract_spec_tests.exs`. The suite now contains 4,868 vectors (2,162 `ok` / 2,706 `fail`) and all pass with bit-for-bit parity to the pinned `wasm-tools` 1.255.0, with no known failures.
- v128 load/store natural alignments corrected for `splat`/`extend`/`zero` variants.
- `v128.const` f32x4/f64x2 lanes and `f32.const`/`f64.const` now round decimal and hex literals through exact rational arithmetic, including overflow-to-max-finite handling.
- Bare `-0` is lexed as a float literal so negative zero keeps its sign bit (integers have no signed zero).
- Decimal integer detection requires at least one digit, so annotation atoms such as `--` no longer raise.
- Identifiers and atoms no longer swallow trailing `;;` line comments.
- Trailing newlines are normalized on every generated `.wat` vector.

## [0.6.0] - 2026-08-03

### Added

- Full Garbage Collection proposal support: recursive/sub-typed type groups, struct and array types, and GC/ref instructions.
- `br_on_cast` / `br_on_cast_fail` cast immediates with source/target nullability flags.
- GC array operations: `array.new`, `array.new_default`, `array.new_fixed`, `array.new_data`, `array.new_elem`, `array.get`/`array.get_s`/`array.get_u`, `array.set`, `array.len`, `array.copy`, `array.fill`, `array.init_data`, `array.init_elem`.
- GC/ref valtypes and reference instructions: `ref.null`, `ref.eq`, `ref.test`, `ref.cast`, `any`/`extern` conversions, `ref.i31`, and abstract heap types.
- Implicit func types for untagged functions, reusing declared top-level types only (explicit `rec`-group types are never reused, matching the reference encoder).
- Named struct field index resolution.

### Changed

- Element and table segment encoding aligned byte-for-byte with `wasm-tools`.
- Type-section encoding for recursive groups, `sub` types, and final-subtype markers aligned with `wasm-tools`.
- Full spec-suite parity: all 5,146 official spec vectors now pass against the pinned `wasm-tools` 1.255.0 reference, with no remaining known failures.
- The spec-test harness now treats `fail/` vectors that `wasm-tools` validates as valid as byte-parity cases instead of expecting invalidity.

## [0.5.0] - 2025

### Added

- `wasm-tools` (bytecodealliance) as the spec-test reference encoder and validator, replacing the previous scratch tooling.
- Leading type-use blocktypes; `else` is always emitted explicitly for `if`.

### Changed

- Aligned element-section, import, table, and element/reftype encoding with `wasm-tools` output.
- Reduced the known-failure count substantially across the spec-suite vectors.

## [0.4.0] - 2025

### Added

- `Watusi.Patcher` for binary patching of pre-compiled WASM templates (data segments and global initializers).

## [0.2.0] - 2025

### Added

- Fixed-width SIMD extension and endianflip test coverage.
- Threads/Atomics extension, sign-extension operators, and the `name` custom section.
- Multi-value blocks and Tail Calls.
- Reference Types support with dbg-stories samples.
- Inline import support and expanded SIMD instruction coverage.

### Changed

- Refactored the encoder into submodules.
- Completed floating-point support with bit-accurate NaN and hex float handling.

## [0.1.0] - 2025

### Added

- Initial Watusi release with bit-for-bit WASM parity for Core 1.0.
- Core 1.0 and Bulk Memory Operations support.
- `start` section encoding.
- Unicode and hex string escape support.
- Floating-point support.
- Native Elixir WAT-to-WASM conversion pipeline.

[Unreleased]: https://github.com/mwmiller/watusi/compare/v0.6.1...HEAD
[0.6.1]: https://github.com/mwmiller/watusi/releases/tag/v0.6.1
[0.6.0]: https://github.com/mwmiller/watusi/releases/tag/v0.6.0
[0.5.0]: https://github.com/mwmiller/watusi/releases/tag/v0.5.0
[0.4.0]: https://github.com/mwmiller/watusi/releases/tag/v0.4.0
[0.2.0]: https://github.com/mwmiller/watusi/releases/tag/v0.2.0
[0.1.0]: https://github.com/mwmiller/watusi/releases/tag/v0.1.0
