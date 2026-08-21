defmodule Watusi.NameParityTest do
  use ExUnit.Case, async: true
  import Watusi.TestHelper

  @spec_vectors_dir "test/spec_vectors"

  for test_group_dir <- Path.wildcard(Path.join(@spec_vectors_dir, "*")),
      File.dir?(test_group_dir) do
    group_name = Path.basename(test_group_dir)

    for path <- Path.wildcard(Path.join(test_group_dir, "ok/*.wat")) do
      @path path
      @test_name "#{group_name}: #{Path.basename(path)}"
      @tag :name_parity

      test "name parity: #{@test_name}" do
        wat = File.read!(@path)
        assert_name_parity(wat, @path)
      end
    end
  end
end
