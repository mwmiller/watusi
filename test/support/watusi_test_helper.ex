defmodule Watusi.TestHelper do
  @moduledoc """
  Helper functions for Watusi integration tests.
  """
  import ExUnit.Assertions

  @doc """
  Compares Watusi's output for a given WAT string against the official `wat2wasm` tool
  and ensures it passes `wasm-validate`.
  """
  def assert_wasm_parity(wat, filename_prefix \\ "test_temp") do
    watusi_wasm = Watusi.to_wasm(wat)

    # 1. Validate with wasm-validate
    validate_wasm(watusi_wasm, "#{filename_prefix}.wasm")

    # 2. Compare against wat2wasm
    expected_wasm =
      compile_with_wabt(wat, "#{filename_prefix}.wat", "#{filename_prefix}_ref.wasm")

    assert watusi_wasm == expected_wasm
  end

  defp validate_wasm(binary, path) do
    File.write!(path, binary)

    try do
      case System.cmd("wasm-validate", [
             "--enable-all",
             path
           ]) do
        {_output, 0} -> :ok
        {output, _} -> flunk("Generated WASM failed wasm-validate:
#{output}")
      end
    after
      File.rm(path)
    end
  end

  defp compile_with_wabt(wat, wat_path, wasm_path) do
    File.write!(wat_path, wat)

    try do
      case System.cmd("wat2wasm", [
             "--enable-all",
             wat_path,
             "-o",
             wasm_path
           ]) do
        {_output, 0} ->
          File.read!(wasm_path)

        {output, _} ->
          flunk("wat2wasm failed to compile reference WAT:
#{output}")
      end
    after
      File.rm(wat_path)
      if File.exists?(wasm_path), do: File.rm(wasm_path)
    end
  end
end
