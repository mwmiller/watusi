defmodule Watusi.PatcherTest do
  use ExUnit.Case, async: true

  defp validate_with_wabt(wasm_binary) do
    tmp = System.tmp_dir!()
    path = Path.join(tmp, "test_#{:rand.uniform(999_999)}.wasm")
    File.write!(path, wasm_binary)

    try do
      case System.cmd("wasm-validate", [path], stderr_to_stdout: true) do
        {_output, 0} -> :ok
        {output, _} -> {:error, output}
      end
    after
      File.rm(path)
    end
  end

  describe "data segment patching" do
    test "patches data segment at offset 0" do
      # Create a simple module with data
      wat = """
      (module
        (memory 1)
        (data (i32.const 0) "Hello")
      )
      """

      original = Watusi.to_wasm(wat)

      # Patch with new data
      patched =
        Watusi.Patcher.patch(original,
          data: [{0, "World"}]
        )

      # Should still be valid
      assert :ok = validate_with_wabt(patched)
      assert byte_size(patched) > 0
    end

    test "patches multiple data segments" do
      wat = """
      (module
        (memory 1)
        (data (i32.const 0) "AAA")
        (data (i32.const 100) "BBB")
        (data (i32.const 200) "CCC")
      )
      """

      original = Watusi.to_wasm(wat)

      patched =
        Watusi.Patcher.patch(original,
          data: [
            {0, "XXX"},
            {100, "YYY"},
            {200, "ZZZ"}
          ]
        )

      assert :ok = validate_with_wabt(patched)
    end

    test "patches large data segment" do
      wat = """
      (module
        (memory 10)
        (data (i32.const 0) "placeholder")
      )
      """

      original = Watusi.to_wasm(wat)

      # 512KB data
      large_data = :binary.copy(<<0xFF>>, 512 * 1024)

      patched =
        Watusi.Patcher.patch(original,
          data: [{0, large_data}]
        )

      assert :ok = validate_with_wabt(patched)
    end

    test "patches data at high memory addresses" do
      wat = """
      (module
        (memory 10)
        (data (i32.const 0x80000) "data_a")
        (data (i32.const 0x81000) "data_b")
      )
      """

      original = Watusi.to_wasm(wat)

      patched =
        Watusi.Patcher.patch(original,
          data: [
            {0x80000, "PATCHED_A"},
            {0x81000, "PATCHED_B"}
          ]
        )

      assert :ok = validate_with_wabt(patched)
    end
  end

  describe "global patching" do
    test "patches i32 global initial value" do
      wat = """
      (module
        (global $version (mut i32) (i32.const 0))
        (func (export "get_version") (result i32)
          global.get $version)
      )
      """

      original = Watusi.to_wasm(wat)

      # Patch global 0 to value 5
      patched =
        Watusi.Patcher.patch(original,
          globals: %{0 => 5}
        )

      assert :ok = validate_with_wabt(patched)
    end

    test "patches multiple globals" do
      wat = """
      (module
        (global $version (mut i32) (i32.const 0))
        (global $base (mut i32) (i32.const 0))
        (global $offset (mut i32) (i32.const 0))
      )
      """

      original = Watusi.to_wasm(wat)

      patched =
        Watusi.Patcher.patch(original,
          globals: %{
            0 => 5,
            1 => 0x1234,
            2 => 0xABCD
          }
        )

      assert :ok = validate_with_wabt(patched)
    end

    test "patches globals with large values" do
      wat = """
      (module
        (global $big (mut i32) (i32.const 0))
      )
      """

      original = Watusi.to_wasm(wat)

      patched =
        Watusi.Patcher.patch(original,
          globals: %{0 => 0x7FFFFFFF}
        )

      assert :ok = validate_with_wabt(patched)
    end
  end

  describe "combined patching" do
    test "patches both data and globals" do
      wat = """
      (module
        (memory 1)
        (global $g0 (mut i32) (i32.const 0))
        (data (i32.const 0) "data")
      )
      """

      original = Watusi.to_wasm(wat)

      patched =
        Watusi.Patcher.patch(original,
          data: [{0, "STORY_DATA"}],
          globals: %{0 => 5}
        )

      assert :ok = validate_with_wabt(patched)
    end

    test "patches multiple data segments and globals" do
      # Template with multiple data segments and globals
      wat = """
      (module
        (memory 10)
        (global $g0 (mut i32) (i32.const 0))
        (global $g1 (mut i32) (i32.const 0))
        (global $g2 (mut i32) (i32.const 0))
        (global $g3 (mut i32) (i32.const 0))
        (global $g4 (mut i32) (i32.const 0))
        
        (data (i32.const 0x00000) "placeholder_0")
        (data (i32.const 0x10000) "placeholder_1")
        (data (i32.const 0x20000) "placeholder_2")
        (data (i32.const 0x30000) "placeholder_3")
        (data (i32.const 0x40000) "placeholder_4")
        
        (func (export "test") (result i32)
          global.get $g0)
      )
      """

      template = Watusi.to_wasm(wat)

      # Patch with runtime data
      data_0 = :binary.copy(<<0x01>>, 1024)
      data_1 = :binary.copy(<<0x02>>, 512)
      data_2 = :binary.copy(<<0x03>>, 256)
      data_3 = :binary.copy(<<0x04>>, 128)
      data_4 = :binary.copy(<<0x05>>, 64)

      patched =
        Watusi.Patcher.patch(template,
          data: [
            {0x00000, data_0},
            {0x10000, data_1},
            {0x20000, data_2},
            {0x30000, data_3},
            {0x40000, data_4}
          ],
          globals: %{
            0 => 42,
            1 => 0x1234,
            2 => 0x5678,
            3 => 0xABCD,
            4 => 1024
          }
        )

      assert :ok = validate_with_wabt(patched)
    end
  end

  describe "edge cases" do
    test "raises on invalid WASM binary" do
      assert_raise ArgumentError, ~r/Invalid WASM binary/, fn ->
        Watusi.Patcher.patch(<<1, 2, 3, 4>>, data: [])
      end
    end

    test "handles module with no data section" do
      wat = """
      (module
        (func (export "noop"))
      )
      """

      original = Watusi.to_wasm(wat)

      # Patching data on module without data section should not crash
      patched =
        Watusi.Patcher.patch(original,
          data: [{0, "test"}]
        )

      assert :ok = validate_with_wabt(patched)
    end

    test "handles module with no globals" do
      wat = """
      (module
        (func (export "noop"))
      )
      """

      original = Watusi.to_wasm(wat)

      patched =
        Watusi.Patcher.patch(original,
          globals: %{0 => 42}
        )

      assert :ok = validate_with_wabt(patched)
    end

    test "handles empty patches" do
      wat = """
      (module
        (memory 1)
        (data (i32.const 0) "test")
      )
      """

      original = Watusi.to_wasm(wat)

      patched = Watusi.Patcher.patch(original, data: [], globals: %{})

      assert :ok = validate_with_wabt(patched)
      # Should be identical
      assert patched == original
    end
  end
end
