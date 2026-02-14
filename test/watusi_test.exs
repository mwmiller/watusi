defmodule WatusiTest do
  use ExUnit.Case
  import Watusi.TestHelper

  test "integration: simple add function" do
    wat = """
    (module
      (func $add (param i32 i32) (result i32)
        local.get 0
        local.get 1
        i32.add
      )
      (export "add" (func $add))
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: multiple functions" do
    wat = """
    (module
      (func $first (result i32)
        i32.const 42
      )
      (func $second (param i32) (result i32)
        local.get 0
        i32.const 1
        i32.add
      )
      (export "first" (func $first))
      (export "second" (func $second))
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: local identifiers" do
    wat = """
    (module
      (func $swap (param $a i32) (param $b i32) (result i32 i32)
        local.get $b
        local.get $a
      )
      (export "swap" (func $swap))
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: imports" do
    wat = """
    (module
      (import "env" "print" (func $print (param i32)))
      (func (export "main")
        i32.const 42
        call $print
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: globals" do
    wat = """
    (module
      (global $g (mut i32) (i32.const 0))
      (func (export "get_g") (result i32)
        global.get $g
      )
      (func (export "set_g") (param i32)
        local.get 0
        global.set $g
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: memory and data" do
    wat = """
    (module
      (memory (export "mem") 1)
      (data (i32.const 0) "hello")
      (func (export "load") (param i32) (result i32)
        local.get 0
        i32.load
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: blocks" do
    wat = """
    (module
      (func (export "test") (result i32)
        (block (result i32)
          i32.const 42
        )
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: floating point" do
    wat = """
    (module
      (func (export "floats") (result f32 f64)
        f32.const 1.234
        f64.const -5.678e10
        return
      )
      (func (export "specials") (result f32 f32 f32)
        f32.const inf
        f32.const -inf
        f32.const nan
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: hex floats" do
    wat = """
    (module
      (func (export "hex_floats") (result f32 f64)
        f32.const 0x1.8p1
        f64.const 0x1p-12
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: float from hex int" do
    wat = """
    (module
      (func (export "hex_int") (result f32)
        f32.const 0x3f800000
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: tables and elements" do
    wat = """
    (module
      (table (export "tab") 2 funcref)
      (func $f1 (result i32) i32.const 1)
      (func $f2 (result i32) i32.const 2)
      (elem (i32.const 0) $f1 $f2)
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: memory offset and align" do
    wat = """
    (module
      (memory 1)
      (func (export "load") (param i32) (result i32)
        local.get 0
        i32.load8_u offset=4 align=1
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: br_table with labels" do
    wat = """
    (module
      (func (export "test") (param i32) (result i32)
        (block $outside (result i32)
          (block $inside (result i32)
            (i32.const 100)
            (local.get 0)
            (br_table $inside $outside $outside)
            (drop)
            (i32.const 1)
          )
          (i32.const 2)
          (i32.add)
        )
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: call_indirect" do
    wat = """
    (module
      (type $binop (func (param i32 i32) (result i32)))
      (table 1 funcref)
      (func $add (type $binop)
        local.get 0
        local.get 1
        i32.add
      )
      (elem (i32.const 0) $add)
      (func (export "test") (param i32 i32) (result i32)
        local.get 0
        local.get 1
        i32.const 0
        call_indirect (type $binop)
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: official fac.wast factorial" do
    wat = """
    (module
      (func (export "fac-rec") (param i64) (result i64)
        (if (result i64) (i64.eq (local.get 0) (i64.const 0))
          (then (i64.const 1))
          (else
            (i64.mul (local.get 0) (call 0 (i64.sub (local.get 0) (i64.const 1))))
          )
        )
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: start section" do
    wat = """
    (module
      (func $init
        i32.const 42
        drop
      )
      (start $init)
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: string hex escapes" do
    wat = """
    (module
      (memory 1)
      (data (i32.const 0) "hello\\0a\\ffworld")
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: comments" do
    wat = """
    (module
      ;; This is a line comment
      (func $test
        i32.const 1 (; this is a block comment ;)
        drop
        (;
           Nested block comments
           (; are also supported ;)
        ;)
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: memory grow and size" do
    wat = """
    (module
      (memory 1)
      (func (export "test") (param i32) (result i32)
        memory.size
        drop
        local.get 0
        memory.grow
        drop
        memory.size
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: unicode escapes" do
    wat = """
    (module
      (memory 1)
      (data (i32.const 0) "rocket \\u{1f680}")
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: bulk memory operations" do
    wat = """
    (module
      (memory 1)
      (data $d1 "hello")
      (func (export "test")
        i32.const 0
        i32.const 0
        i32.const 5
        memory.init $d1
        data.drop $d1
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: advanced floating point" do
    wat = """
    (module
      (func (export "test") (result f32 f32 f32 f32 f64 f64)
        f32.const +inf
        f32.const -inf
        f32.const +nan
        f32.const -nan
        f64.const nan:0x1
        f64.const -nan:0x123456789abcd
      )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: hex floats with signs" do
    wat = """
    (module
      (func (export "test") (result f32 f64)
        f32.const +0x1p+0
        f64.const -0x1.8p-1
      )
    )
    """

    assert_wasm_parity(wat)
  end

  for path <- Path.wildcard("test/samples/*.wat") do
    @sample_path path

    @sample_name Path.basename(path, ".wat")

    test "integration: sample #{@sample_name}.wat" do
      @sample_path |> File.read!() |> assert_wasm_parity(@sample_name)
    end
  end

  test "integration: custom name section" do
    wat = """



    (module $my_module



      (func $add (param $a i32) (param $b i32) (result i32)



        local.get $a



        local.get $b



        i32.add)



    )



    """

    watusi_wasm = Watusi.to_wasm(wat, debug_names: true)

    # Use a modified compilation command for this specific test

    wat_path = "test_name.wat"

    wasm_path = "test_name.wasm"

    File.write!(wat_path, wat)

    expected_wasm =
      try do
        {_output, 0} = System.cmd("wat2wasm", ["--debug-names", wat_path, "-o", wasm_path])

        File.read!(wasm_path)
      after
        File.rm(wat_path)

        File.rm(wasm_path)
      end

    assert watusi_wasm == expected_wasm
  end
end
