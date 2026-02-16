defmodule Mix.Tasks.GenRefs do
  @moduledoc "Generate reference WASM files from WAT files using wat2wasm"
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

    case System.cmd("wat2wasm", ["--enable-all", wat_path, "-o", ref_path],
           stderr_to_stdout: true
         ) do
      {_output, 0} -> IO.write(".")
      {_output, _} -> IO.write("s")
    end
  end
end
