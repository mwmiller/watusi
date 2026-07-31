defmodule Mix.Tasks.GenRefs do
  @moduledoc "Generate reference WASM files from WAT files using wasm-tools"
  @shortdoc "Generate reference WASM files for tests"

  use Mix.Task

  @impl Mix.Task
  def run(_args) do
    "test/spec_vectors"
    |> File.ls!()
    |> Enum.each(fn category ->
      ok_dir = Path.join(["test/spec_vectors", category, "ok"])

      if File.dir?(ok_dir) do
        ok_dir
        |> File.ls!()
        |> Enum.filter(&String.ends_with?(&1, ".wat"))
        |> Enum.each(&generate_reference(ok_dir, &1))
      end
    end)

    IO.puts("\nReference WASM files generated successfully!")
  end

  defp generate_reference(dir, wat_file) do
    wat_path = Path.join(dir, wat_file)
    ref_path = Path.join(dir, String.replace(wat_file, ".wat", ".ref.wasm"))
    stripped_path = "#{ref_path}.tmp"

    # `wasm-tools parse` emits a name section, so strip it back out for
    # byte-level comparison with Watusi's output.
    result =
      with {_output, 0} <-
             System.cmd("wasm-tools", ["parse", wat_path, "-o", ref_path], stderr_to_stdout: true),
           {_output, 0} <-
             System.cmd("wasm-tools", ["strip", "--all", "-o", stripped_path, ref_path],
               stderr_to_stdout: true
             ) do
        File.rename!(stripped_path, ref_path)
        :ok
      else
        {_output, _} -> :error
      end

    case result do
      :ok -> IO.write(".")
      :error -> IO.write("s")
    end
  end
end
