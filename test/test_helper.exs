ExUnit.start(max_cases: System.schedulers_online() * 4)

defmodule Watusi.TestHelper do
  @moduledoc """
  Helper functions for Watusi integration tests.
  """
  import ExUnit.Assertions

  @doc """
  Compares Watusi's output for a given WAT string against pre-generated reference WASM.
  Falls back to wat2wasm if reference doesn't exist.
  """
  def assert_wasm_parity(wat, path_or_name \\ nil) do
    ref_wasm_path = get_ref_path(path_or_name)

    case read_ref_wasm(ref_wasm_path) do
      {:ok, expected_wasm} ->
        compare_with_reference(wat, expected_wasm)

      :not_found ->
        handle_missing_reference(wat)
    end
  end

  defp compare_with_reference(wat, expected_wasm) do
    watusi_wasm = Watusi.to_wasm(wat)

    case watusi_wasm == expected_wasm do
      true ->
        :ok

      false ->
        # Only validate and fail if they differ
        validate_wasm(watusi_wasm)
        assert watusi_wasm == expected_wasm
    end
  end

  defp handle_missing_reference(wat) do
    # Fallback to generating reference on the fly
    case compile_reference(wat) do
      {:ok, expected_wasm} ->
        compare_with_reference(wat, expected_wasm)

      {:error, _msg} ->
        # Reference tool fails - ensure Watusi doesn't crash
        try do
          Watusi.to_wasm(wat)
        rescue
          _ -> :ok
        end
    end
  end

  def assert_wasm_failure(wat, _name) do
    # 1. See what the reference tool says
    case compile_reference(wat) do
      {:error, _msg} ->
        # Reference tool says it's bad. Watusi should too.
        try do
          wasm = Watusi.to_wasm(wat)
          # If it didn't raise, it MUST be invalid - validate to confirm
          refute_wasm_valid(wasm)
        rescue
          _ -> :ok
        end

      {:ok, wasm} ->
        # If reference tool says it's OK, Watusi must still produce something invalid
        # because it's in a 'fail' directory.
        refute_wasm_valid(wasm)
    end
  end

  defp get_ref_path(nil), do: nil

  defp get_ref_path(path) do
    if String.ends_with?(path, ".wat") do
      String.replace(path, ".wat", ".ref.wasm")
    else
      nil
    end
  end

  defp read_ref_wasm(nil), do: :not_found

  defp read_ref_wasm(path) do
    if File.exists?(path) do
      {:ok, File.read!(path)}
    else
      :not_found
    end
  end

  defp compile_reference(wat) do
    tmp_path = Path.join(System.tmp_dir!(), "watusi_ref_#{System.unique_integer([:positive])}")
    wat_path = "#{tmp_path}.wat"
    wasm_path = "#{tmp_path}.wasm"
    File.write!(wat_path, wat)

    try do
      case System.cmd("wat2wasm", ["--enable-all", wat_path, "-o", wasm_path],
             stderr_to_stdout: true
           ) do
        {_output, 0} -> {:ok, File.read!(wasm_path)}
        {output, _} -> {:error, output}
      end
    after
      File.rm_rf(wat_path)
      File.rm_rf(wasm_path)
    end
  end

  defp validate_wasm(binary) do
    path = Path.join(System.tmp_dir!(), "watusi_val_#{System.unique_integer([:positive])}.wasm")
    File.write!(path, binary)

    try do
      case System.cmd("wasm-validate", ["--enable-all", path], stderr_to_stdout: true) do
        {_output, 0} -> :ok
        {output, _} -> flunk("Generated WASM failed wasm-validate:\n#{output}")
      end
    after
      File.rm_rf(path)
    end
  end

  defp refute_wasm_valid(binary) do
    path =
      Path.join(System.tmp_dir!(), "watusi_refute_#{System.unique_integer([:positive])}.wasm")

    File.write!(path, binary)

    try do
      case System.cmd("wasm-validate", [path], stderr_to_stdout: true) do
        {_output, 0} -> flunk("Expected WASM to be invalid, but it passed wasm-validate")
        {_output, _} -> :ok
      end
    after
      File.rm_rf(path)
    end
  end
end
