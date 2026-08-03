# wasm-tools Spec Vectors

This directory contains the official WebAssembly spec test vectors (from the
[spec repository](https://github.com/WebAssembly/spec)) used to validate
Watusi's WAT → WASM encoder.

## Layout

Vectors are organized into one directory per proposal or feature. Each feature directory contains two
subdirectories:

- `ok/`   – modules expected to compile to **valid** WASM.
- `fail/` – modules expected to be **rejected** (they contain invalid WASM and must not encode cleanly).

There are 2,162 `ok` vectors and 2,706 `fail` vectors, for a total of 4,868 vectors.

## Provenance

The vectors are generated from the upstream
[WebAssembly/spec](https://github.com/WebAssembly/spec) test suite by
`scripts/extract_spec_tests.exs`, which splits each `.wast` file into one `.wat`
module per `(module ...)` form. This refresh is pinned to upstream commit:

- `bdd7164bfe18cf0bd5c3d90ef8cc3b8919fb9c0a` (2026-07-28, "[spectec] Upstream
  tweaks to Wasm 1.0 rules (#2218)")

Source files are all 258 `test/core/*.wast` files (flat files plus the
`bulk-memory/`, `exceptions/`, `gc/`, `memory64/`, `multi-memory/`,
`relaxed-simd/`, and `simd/` proposal subdirectories), keyed by basename.
`memory_grow.wast` exists in both the core root and `multi-memory/` and is
merged into one directory. Every emitted `.wat` ends with a single trailing
newline, which upstream sources do not always carry.

Compared with the previous vectors, upstream cleanup reduced the standalone
`ok` module count (e.g. `imports.wast` and `linking.wast` now express most
cases as `assert_unlinkable`/`assert_invalid` test groups instead of top-level
modules, and the standalone `data1` directory moved into `multi-memory/` where
its forms are `assert_trap`-only). No `fail` vectors were lost.

## How the tests work

`test/spec_test.exs` discovers every `.wat` file and generates one ExUnit test per file:

- For `ok/<name>.wat`, the test compiles the module with Watusi and compares the result **byte-for-byte**
  against a reference binary produced by `wasm-tools` (`wasm-tools parse` piped through
  `wasm-tools strip --all`, so both sides have the name section removed).
- For `fail/<name>.wat`, the test asserts that the module does **not** produce a valid WASM binary
  (Watusi's output is rejected by `wasm-tools validate --features all`).

References are produced **live** at test time by `wasm-tools` via `test/test_helper.exs`
(`handle_missing_reference` / `compile_reference`). Optionally, a pre-built `.ref.wasm` may be committed
next to a `.wat` file; when present it is used as the reference instead of compiling on the fly. Generate
these with `mix gen_refs`. By default no `.ref.wasm` files are committed, so references are compiled live.

## Known failures

Vectors that fail on the current toolchain are listed in `test/known_failures.txt`; `test/spec_test.exs` tags
matching tests with `@tag :known_failure`, and because `test/test_helper.exs` starts ExUnit with
`exclude: [:known_failure]`, they are skipped by default so the suite stays green. There are currently **no**
known failures: the full suite (including `:known_failure`-tagged vectors) passes `4868/4868`.

For `fail/` vectors, the harness asserts byte-parity with the wasm-tools reference. The pinned `wasm-tools`
1.255.0 validates some modules the spec suite marks invalid (e.g. `br_on_cast`/`br_on_cast_fail` to
nullable/cross-heaptype targets and certain `tag` exceptions); when the reference accepts such a vector, the
harness treats the reference bytes as ground truth and requires Watusi to match them exactly.

Run the full suite including the known failures:

```sh
mix test --include known_failure
```

## Tooling requirements

The spec suite requires [wasm-tools](https://github.com/bytecodealliance/wasm-tools) on your `PATH`:

- `wasm-tools` – reference encoder/decoder (`parse`, `strip --all`) and validator
  (`validate --features all`).

References and validation are produced against the toolchain version in use. Parity to `wasm-tools` is
assured for that version; a different `wasm-tools` build that changes binary output will require re-verifying
the vectors. The versions used for the documented results are:

| Tool | Version |
|------|---------|
| `wasm-tools` | 1.255.0 |
| Elixir | 1.20.2 |
| Erlang/OTP | 29 |

Note: the suite covers every proposal the installed `wasm-tools` parses. The handful of vectors Watusi still
cannot encode are listed in `test/known_failures.txt` and are skipped by default.
