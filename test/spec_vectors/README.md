# Spec Vectors

This directory contains the official WebAssembly spec test vectors (from the
[spec repository](https://github.com/WebAssembly/spec)) used to validate
Watusi's WAT → WASM encoder.

## Layout

Vectors are organized into one directory per proposal or feature. Each feature directory contains two
subdirectories:

- `ok/`   – modules expected to compile to **valid** WASM.
- `fail/` – modules expected to be **rejected** (they contain invalid WASM and must not encode cleanly).

## Provenance

The vectors are generated from the upstream
[WebAssembly/spec](https://github.com/WebAssembly/spec) test suite by
`scripts/extract_spec_tests.exs`, which splits each `.wast` file into one `.wat`
module per `(module ...)` form. To refresh the vectors, fetch the desired
upstream `test/core/*.wast` files (flat files plus any proposal subdirectories
such as `bulk-memory/`, `exceptions/`, `gc/`, `memory64/`, `multi-memory/`,
`relaxed-simd/`, and `simd/`) and run the extractor once per file:

```sh
elixir scripts/extract_spec_tests.exs <in.wast> test/spec_vectors/<group>
```

This writes `<group>/ok/module_N.wat` and `<group>/fail/module_N.wat`. Every
emitted `.wat` ends with a single trailing newline, which upstream sources do
not always carry.

## How the tests work

`test/spec_test.exs` discovers every `.wat` file and generates one ExUnit test per file:

- For `ok/<name>.wat`, the test compiles the module with Watusi and compares the result **byte-for-byte**
  against a reference binary produced by `wasm-tools` (`wasm-tools parse` piped through
  `wasm-tools strip --all`, so both sides have the name section removed).
- For `fail/<name>.wat`, the test asserts that the module does **not** produce a valid WASM binary
  (Watusi's output is rejected by `wasm-tools validate --features all`). The reference toolchain is
  more permissive than the spec suite for some constructs; when it accepts such a vector, the harness
  treats the reference bytes as ground truth and requires byte-parity instead of a refusal.

References are produced **live** at test time by `wasm-tools` via `test/test_helper.exs`
(`handle_missing_reference` / `compile_reference`). Optionally, a pre-built `.ref.wasm` may be committed
next to a `.wat` file; when present it is used as the reference instead of compiling on the fly. Generate
these with `mix gen_refs`. By default no `.ref.wasm` files are committed, so references are compiled live.

## Known failures

Vectors that fail on the current toolchain are listed in `test/known_failures.txt`; `test/spec_test.exs` tags
matching tests with `@tag :known_failure`, and because `test/test_helper.exs` starts ExUnit with
`exclude: [:known_failure]`, they are skipped by default so the suite stays green. Run the full suite
including the known failures:

```sh
mix test --include known_failure
```

After upgrading the `wasm-tools` reference toolchain or refreshing the vectors, re-run the full suite
(including tagged tests) and update `known_failures.txt` to match observed results.

## Name-section parity

`test/name_parity_test.exs` verifies the optional `debug_names: true` output: every `name`
custom-section subsection Watusi emits must match the unstripped `wasm-tools parse` reference
byte-for-byte. These tests are tagged `:name_parity` and excluded by default because they roughly
double suite runtime:

```sh
mix test --include name_parity
```

## Tooling requirements

The spec suite requires [wasm-tools](https://github.com/bytecodealliance/wasm-tools) on your `PATH`:

- `wasm-tools` – reference encoder/decoder (`parse`, `strip --all`) and validator
  (`validate --features all`).

References and validation are produced against the toolchain version in use. Parity to `wasm-tools` is
assured for that version; a different `wasm-tools` build that changes binary output will require re-verifying
the vectors (`mix gen_refs` followed by `mix test`).

Note: the suite covers every proposal the installed `wasm-tools` parses. Vectors Watusi cannot yet encode
are listed in `test/known_failures.txt` and skipped by default.
