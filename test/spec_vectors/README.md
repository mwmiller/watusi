# Test Reference Files

This directory contains pre-generated reference WASM files (`.ref.wasm`) that are used for testing.

## Generating Reference Files

To regenerate all reference files:

```bash
elixir test/generate_reference_wasm.exs
```

This script:
- Processes all `.wat` files in `test/spec_vectors/*/ok/`
- Generates corresponding `.ref.wasm` files using `wat2wasm --enable-all`
- Skips files that `wat2wasm` cannot parse (shown as 's' in output)

## Why Pre-generate?

Pre-generating reference files significantly speeds up test execution:
- **Before**: ~200 seconds (running `wat2wasm` for each test)
- **After**: ~130 seconds (reading pre-generated files)

The reference files are committed to the repository so tests can run without requiring `wat2wasm` to be installed (though `wasm-validate` is still needed for validation).

## Test Behavior

The test helper (`test/support/watusi_test_helper.ex`) will:
1. First try to load the pre-generated `.ref.wasm` file
2. Fall back to running `wat2wasm` if the reference file doesn't exist
3. Compare Watusi's output against the reference
4. Validate both outputs with `wasm-validate`
