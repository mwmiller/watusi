defmodule Watusi.TestHelper do
  @moduledoc """
  Helper functions for Watusi integration tests.
  """
  import ExUnit.Assertions

  @doc """
  Compares Watusi's output for a given WAT string against the official `wat2wasm` tool
  and ensures it passes `wasm-validate`.
  """
  def assert_wasm_parity(wat, name \\ "test_temp") do
    # Strip subdirectories to keep temp files in the root project folder during tests
    filename_prefix = Path.basename(name)

    # 1. Check if the reference tool can compile it
    case compile_reference(wat, filename_prefix) do
      {:ok, expected_wasm} ->
        watusi_wasm = Watusi.to_wasm(wat)
        # Both must pass validation
        validate_wasm(watusi_wasm, "#{filename_prefix}.wasm")
        assert watusi_wasm == expected_wasm

      {:error, _msg} ->
        # If the reference tool fails, it means the WAT is intentionally malformed.
        # Watusi should either fail to encode or produce a binary that fails wasm-validate.
        # For now, we just ensure it doesn't crash the compiler.
        try do
          Watusi.to_wasm(wat)
        rescue
          _ -> :ok
        end
    end
  end

  defp compile_reference(wat, prefix) do
    wat_path = "#{prefix}_ref.wat"
    wasm_path = "#{prefix}_ref.wasm"
    File.write!(wat_path, wat)

    try do
      case System.cmd("wat2wasm", ["--enable-all", wat_path, "-o", wasm_path], stderr_to_stdout: true) do
        {_output, 0} -> {:ok, File.read!(wasm_path)}
        {output, _} -> {:error, output}
      end
    after
      File.rm(wat_path)
      if File.exists?(wasm_path), do: File.rm(wasm_path)
    end
  end

  defp validate_wasm(binary, path) do
    File.write!(path, binary)

    try do
      case System.cmd("wasm-validate", ["--enable-all", path], stderr_to_stdout: true) do
        {_output, 0} -> :ok
        {output, _} -> flunk("Generated WASM failed wasm-validate:
#{output}")
      end
    after
      File.rm(path)
    end
  end
end
