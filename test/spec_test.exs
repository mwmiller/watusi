defmodule Watusi.SpecTest do
  use ExUnit.Case, async: true
  import Watusi.TestHelper

  @spec_vectors_dir "test/spec_vectors"

  # Paths of spec tests that are known to fail on the current toolchain
  # (e.g. missing proposal support in the installed wat2wasm 1.0.41).
  # These are excluded from default test runs via the :known_failure tag.
  # Regenerate with: mix run /tmp/genfail*.exs-style script that replays
  # assert_wasm_parity / assert_wasm_failure and lists failing .wat paths.
  @known_failures Path.join(__DIR__, "known_failures.txt")
                  |> File.read!()
                  |> String.split("\n", trim: true)
                  |> MapSet.new()

  for test_group_dir <- Path.wildcard(Path.join(@spec_vectors_dir, "*")),
      File.dir?(test_group_dir) do
    group_name = Path.basename(test_group_dir)

    # Success cases
    for path <- Path.wildcard(Path.join(test_group_dir, "ok/*.wat")) do
      @path path
      @test_name "#{group_name}: #{Path.basename(path)}"

      if MapSet.member?(@known_failures, path) do
        @tag :known_failure
      end

      test "spec ok: #{@test_name}" do
        wat = File.read!(@path)
        assert_wasm_parity(wat, @path)
      end
    end

    # Failure cases
    for path <- Path.wildcard(Path.join(test_group_dir, "fail/*.wat")) do
      @path path
      @test_name "#{group_name}: #{Path.basename(path)}"

      if MapSet.member?(@known_failures, path) do
        @tag :known_failure
      end

      test "spec fail: #{@test_name}" do
        wat = File.read!(@path)
        assert_wasm_failure(wat, @path)
      end
    end
  end
end
