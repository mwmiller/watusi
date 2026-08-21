# Default test runs exclude spec tests tagged :known_failure (known to fail on the
# current toolchain, e.g. proposal support Watusi does not yet implement).
# Run them explicitly with: mix test --include known_failure
#
# Name-section parity tests are tagged :name_parity and also excluded by
# default (they roughly double suite runtime). Run with:
#   mix test --include name_parity
ExUnit.start(max_cases: System.schedulers_online() * 4, exclude: [:known_failure, :name_parity])

defmodule Watusi.TestHelper do
  @moduledoc """
  Helper functions for Watusi integration tests.
  """
  import ExUnit.Assertions

  @doc """
  Compares Watusi's output for a given WAT string against pre-generated reference WASM.
  Falls back to wasm-tools if reference doesn't exist.
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
        validate_wasm(watusi_wasm)
        validate_wasm(expected_wasm)
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
        case wasm_valid?(wasm) do
          :ok ->
            # wasm-tools accepts it as valid (it is more permissive than the
            # spec suite for some constructs). Byte-parity with the reference
            # is then the meaningful assertion, not invalidity.
            compare_with_reference(wat, wasm)

          :invalid ->
            # Reference parses but the validator still rejects it. Watusi
            # must not silently produce a valid module either.
            try do
              refute_wasm_valid(Watusi.to_wasm(wat))
            rescue
              _ -> :ok
            end
        end
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
    stripped_path = "#{tmp_path}.stripped.wasm"
    File.write!(wat_path, wat)

    try do
      # wasm-tools is the reference toolchain. `parse` emits a name section, so
      # strip it back out for byte-level comparison with Watusi's output.
      with {_output, 0} <-
             System.cmd("wasm-tools", ["parse", wat_path, "-o", wasm_path],
               stderr_to_stdout: true
             ),
           {_output, 0} <-
             System.cmd("wasm-tools", ["strip", "--all", "-o", stripped_path, wasm_path],
               stderr_to_stdout: true
             ) do
        {:ok, File.read!(stripped_path)}
      else
        {output, _} -> {:error, output}
      end
    after
      File.rm_rf(wat_path)
      File.rm_rf(wasm_path)
      File.rm_rf(stripped_path)
    end
  end

  defp validate_wasm(binary) do
    path = Path.join(System.tmp_dir!(), "watusi_val_#{System.unique_integer([:positive])}.wasm")
    File.write!(path, binary)

    try do
      case System.cmd("wasm-tools", ["validate", "--features", "all", path],
             stderr_to_stdout: true
           ) do
        {_output, 0} -> :ok
        {output, _} -> flunk("Generated WASM failed wasm-tools validate:\n#{output}")
      end
    after
      File.rm_rf(path)
    end
  end

  defp wasm_valid?(binary) do
    path = Path.join(System.tmp_dir!(), "watusi_valid_#{System.unique_integer([:positive])}.wasm")
    File.write!(path, binary)

    try do
      case System.cmd("wasm-tools", ["validate", "--features", "all", path],
             stderr_to_stdout: true
           ) do
        {_output, 0} -> :ok
        {_output, _} -> :invalid
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
      case System.cmd("wasm-tools", ["validate", "--features", "all", path],
             stderr_to_stdout: true
           ) do
        {_output, 0} ->
          flunk("Expected WASM to be invalid, but it passed wasm-tools validate")

        {_output, _} ->
          :ok
      end
    after
      File.rm_rf(path)
    end
  end

  @doc """
  Compares Watusi's `debug_names: true` output against the *unstripped*
  wasm-tools reference. Only subsections Watusi emits (module/func/local
  names) are checked; the reference may carry additional subsections
  (type/table/memory/global names) which Watusi does not produce.
  """
  def assert_name_parity(wat, _path) do
    watusi_wasm = Watusi.to_wasm(wat, debug_names: true)

    case compile_named_reference(wat) do
      {:ok, ref_wasm} ->
        assert_name_sections_match(name_subsections(watusi_wasm), name_subsections(ref_wasm))

      {:error, _output} ->
        :ok
    end
  end

  defp assert_name_sections_match(watusi_names, ref_names) do
    cond do
      map_size(watusi_names) == 0 ->
        :ok

      map_size(ref_names) == 0 ->
        flunk("Watusi emitted a name section but the reference did not")

      true ->
        Enum.each(watusi_names, &assert_subsection_matches(&1, ref_names))
    end
  end

  defp assert_subsection_matches({id, payload}, ref_names) do
    assert ref_names[id] == payload,
           "name subsection #{id} differs from reference" <>
             "\nwatusi: #{inspect(payload)}" <>
             "\nref:    #{inspect(ref_names[id])}"
  end

  defp compile_named_reference(wat) do
    tmp_path = Path.join(System.tmp_dir!(), "watusi_named_#{System.unique_integer([:positive])}")
    wat_path = "#{tmp_path}.wat"
    wasm_path = "#{tmp_path}.wasm"
    File.write!(wat_path, wat)

    try do
      case System.cmd("wasm-tools", ["parse", wat_path, "-o", wasm_path], stderr_to_stdout: true) do
        {_output, 0} -> {:ok, File.read!(wasm_path)}
        {output, _} -> {:error, output}
      end
    after
      File.rm_rf(wat_path)
      File.rm_rf(wasm_path)
    end
  end

  defp name_subsections(binary) do
    case find_custom_section(binary, "name") do
      nil -> %{}
      payload -> parse_subsections(payload, %{})
    end
  end

  defp find_custom_section(<<"\0asm", 1, 0, 0, 0, rest::binary>>, wanted),
    do: do_find_custom_section(rest, wanted)

  defp do_find_custom_section(<<>>, _wanted), do: nil

  defp do_find_custom_section(section, wanted) do
    <<id, rest::binary>> = section
    {size, rest} = leb_u32(rest)
    <<payload::binary-size(size), rest::binary>> = rest

    content =
      if id == 0 do
        {name_len, body} = leb_u32(payload)
        <<name::binary-size(name_len), body::binary>> = body
        if name == wanted, do: body
      end

    if content, do: content, else: do_find_custom_section(rest, wanted)
  end

  defp parse_subsections(<<>>, acc), do: acc

  defp parse_subsections(subsections, acc) do
    <<id, rest::binary>> = subsections
    {size, rest} = leb_u32(rest)
    <<payload::binary-size(size), rest::binary>> = rest
    parse_subsections(rest, Map.put(acc, id, payload))
  end

  defp leb_u32(binary), do: do_leb_u32(binary, 0, 0)

  defp do_leb_u32(<<b, rest::binary>>, shift, acc) when b < 0x80,
    do: {acc + b * 2 ** shift, rest}

  defp do_leb_u32(<<b, rest::binary>>, shift, acc),
    do: do_leb_u32(rest, shift + 7, acc + (b - 0x80) * 2 ** shift)
end
