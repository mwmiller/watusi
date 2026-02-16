defmodule Watusi.SpecTest do
  use ExUnit.Case, async: true
  import Watusi.TestHelper

  @spec_vectors_dir "test/spec_vectors"

  for test_group_dir <- Path.wildcard(Path.join(@spec_vectors_dir, "*")),
      File.dir?(test_group_dir) do
    group_name = Path.basename(test_group_dir)

    # Success cases
    for path <- Path.wildcard(Path.join(test_group_dir, "ok/*.wat")) do
      @path path
      @test_name "#{group_name}: #{Path.basename(path)}"

      test "spec ok: #{@test_name}" do
        wat = File.read!(@path)
        assert_wasm_parity(wat, @path)
      end
    end

    # Failure cases
    for path <- Path.wildcard(Path.join(test_group_dir, "fail/*.wat")) do
      @path path
      @test_name "#{group_name}: #{Path.basename(path)}"

      test "spec fail: #{@test_name}" do
        wat = File.read!(@path)
        assert_wasm_failure(wat, @path)
      end
    end
  end
end
