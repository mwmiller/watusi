# WABT Spec Vectors

This directory contains the official [WebAssembly Binary Toolkit (WABT)](https://github.com/WebAssembly/wabt)
spec test vectors used to validate Watusi's WAT → WASM encoder.

## Layout

Vectors are organized into one directory per proposal or feature. Each feature directory contains two
subdirectories:

- `ok/`   – modules expected to compile to **valid** WASM.
- `fail/` – modules expected to be **rejected** (they contain invalid WASM and must not encode cleanly).

There are 2,380 `ok` vectors and 2,683 `fail` vectors, for a total of 5,063 vectors.

## How the tests work

`test/spec_test.exs` discovers every `.wat` file and generates one ExUnit test per file:

- For `ok/<name>.wat`, the test compiles the module with Watusi and compares the result **byte-for-byte**
  against a reference binary produced by `wat2wasm` (`--enable-all`).
- For `fail/<name>.wat`, the test asserts that the module does **not** produce a valid WASM binary
  (Watusi's output is rejected by `wasm-validate`).

References are produced **live** at test time by `wat2wasm` via `test/test_helper.exs`
(`handle_missing_reference` / `compile_reference`). Optionally, a pre-built `.ref.wasm` may be committed
next to a `.wat` file; when present it is used as the reference instead of compiling on the fly. Generate
these with `mix gen_refs`. By default no `.ref.wasm` files are committed, so references are compiled live.

## Known failures

A small subset of vectors exercises features Watusi does not yet fully support. These paths are listed in
`test/known_failures.txt`; `test/spec_test.exs` tags matching tests with `@tag :known_failure`, and because
`test/test_helper.exs` starts ExUnit with `exclude: [:known_failure]`, they are skipped by default so the
suite stays green. There are currently **75** such vectors, mostly covering Reference-Types and
Function-References.

Run the full suite including the known failures:

```sh
mix test --include known_failure
```

## Tooling requirements

The spec suite requires [WABT](https://github.com/WebAssembly/wabt) on your `PATH`:

- `wat2wasm`     – reference encoder (invoked with `--enable-all`).
- `wasm-validate` – used to confirm that `fail/` vectors produce invalid binaries.

Note: the suite only exercises proposals that the local `wat2wasm --enable-all` can compile. Vectors for
proposals the toolchain does not enable here (for example the Garbage Collection proposal) are skipped and
are therefore not validated by the suite.
