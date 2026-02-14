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

  test "integration: sample add.wat" do
    wat = """
    (module
        (func $add (export "add") (param $a i32) (param $b i32) (result i32)
            (i32.add (local.get $a) (local.get $b))
        )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: sample recursion.wat" do
    wat = """
    (module
        (func $factorial (export "factorial") (param $n i32) (result i32)
            (if (result i32)
                (i32.le_s (local.get $n) (i32.const 1))
                (then (i32.const 1))
                (else
                    (i32.mul
                        (local.get $n)
                        (call $factorial (i32.sub (local.get $n) (i32.const 1)))))
            )
        )

        (func $is_even (export "is_even") (param $n i32) (result i32)
            (if (result i32)
                (i32.eqz (local.get $n))
                (then (i32.const 1))
                (else (call $is_odd (i32.sub (local.get $n) (i32.const 1))))
            )
        )

        (func $is_odd (export "is_odd") (param $n i32) (result i32)
            (if (result i32)
                (i32.eqz (local.get $n))
                (then (i32.const 0))
                (else (call $is_even (i32.sub (local.get $n) (i32.const 1))))
            )
        )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: sample itoa.wat" do
    wat = """
    (module
        (import "env" "log" (func $log (param i32)))
        (memory (export "memory") 1)
        (data (i32.const 8000) "0123456789")
        (global $itoa_out_buf i32 (i32.const 8010))

        (func $itoa (export "itoa") (param $num i32) (result i32 i32)
            (local $numtmp i32)
            (local $numlen i32)
            (local $writeidx i32)
            (local $digit i32)
            (local $dchar i32)

            (i32.lt_s (local.get $num) (i32.const 10))
            if
                (local.set $numlen (i32.const 1))
            else
                (local.set $numlen (i32.const 0))
                (local.set $numtmp (local.get $num))
                (loop $countloop (block $breakcountloop
                    (i32.eqz (local.get $numtmp))
                    br_if $breakcountloop

                    (local.set $numtmp (i32.div_u (local.get $numtmp) (i32.const 10)))
                    (local.set $numlen (i32.add (local.get $numlen) (i32.const 1)))
                    br $countloop
                ))
            end

            (local.set $writeidx
                (i32.sub
                    (i32.add (global.get $itoa_out_buf) (local.get $numlen))
                    (i32.const 1)))

            (loop $writeloop (block $breakwriteloop
                (local.set $digit (i32.rem_u (local.get $num) (i32.const 10)))
                (local.set $dchar (i32.load8_u offset=8000 (local.get $digit)))

                (i32.store8 (local.get $writeidx) (local.get $dchar))

                (local.set $num (i32.div_u (local.get $num) (i32.const 10)))

                (i32.eq (local.get $writeidx) (global.get $itoa_out_buf))
                br_if $breakwriteloop

                (local.set $writeidx (i32.sub (local.get $writeidx) (i32.const 1)))
                br $writeloop
            ))

            (global.get $itoa_out_buf)
            (local.get $numlen)
        )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: sample loops.wat" do
    wat = """
    (module
        (import "env" "buffer" (memory 80))
        (import "env" "log_i32" (func $log_i32 (param i32)))
        (import "env" "rand_i32" (func $rand_i32 (result i32)))

        (func (export "add_all") (param $start i32) (param $count i32) (result i32)
            (local $i i32)
            (local $read_offset i32)
            (local $result i32)

            (local.set $i (i32.const 0))
            (loop $addloop (block $breakaddloop
                (i32.ge_s (local.get $i) (local.get $count))
                br_if $breakaddloop

                (local.set $read_offset
                    (i32.add
                        (local.get $start)
                        (i32.mul (local.get $i) (i32.const 4))))

                (local.set $result
                    (i32.add
                        (local.get $result)
                        (i32.load (local.get $read_offset))))

                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                br $addloop
            ))

            (local.get $result)
        )

        (func (export "rand_multiple_of_10") (result i32)
            (local $n i32)
            (loop $randloop
                (local.set $n (call $rand_i32))
                (i32.ne (i32.rem_u (local.get $n) (i32.const 10)) (i32.const 0))
                br_if $randloop
            )
            (local.get $n)
        )

        (func (export "first_power_over_limit") (param $base i32) (param $limit i32) (result i32)
            (local $n i32)
            (local.set $n (i32.const 1))
            (loop $powerloop (block $breakpowerloop
                (i32.gt_s (local.get $n) (local.get $limit))
                br_if $breakpowerloop
                (local.set $n (i32.mul (local.get $n) (local.get $base)))
                br $powerloop
            ))
            (local.get $n)
        )
    )
    """

    assert_wasm_parity(wat)
  end

  test "integration: sample select.wat" do
    wat = """
    (module
        (func $add_or_sub (export "add_or_sub")
            (param $a i32) (param $b i32) (param $control i32)
            (result i32)
            (select
                (i32.add (local.get $a) (local.get $b))
                (i32.sub (local.get $a) (local.get $b))
                (i32.ge_s (local.get $control) (i32.const 0))
            )
        )
    )
    """

    assert_wasm_parity(wat)
  end
end
