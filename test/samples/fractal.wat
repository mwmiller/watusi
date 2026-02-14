(module

  (import "mem" "pages" (memory 24))

  (global $PALETTE_OFFSET (import "mem" "paletteOffset") i32)

  (global $BAILOUT f32 (f32.const 4.0))
  (global $BLACK i32 (i32.const 0xFF000000))

;; PALETTE
(func $8_bit_clamp
    (param $n i32)
    (param $threshold i32)
    (result i32)

  (local $temp i32)

  (local.set $temp
    (i32.and (i32.add (local.get $n) (local.get $threshold))
             (i32.const 1023)
    )
  )

  ;; How many bits does $temp use?
  (if (result i32)
    ;; 9 or more bits?
    (i32.ge_u (local.get $temp) (i32.const 256))
    (then
      ;; If bit 10 is switched off invert value, else return zero
      (if (result i32 )
        (i32.lt_u (local.get $temp) (i32.const 512))
        (then (i32.sub (i32.const 510) (local.get $temp)))
        (else (i32.const 0))
      )
    )
    (else (local.get $temp))
  )
)
(func $red   (param $iter i32) (result i32) (call $8_bit_clamp (local.get $iter) (i32.const   0)))
(func $green (param $iter i32) (result i32) (call $8_bit_clamp (local.get $iter) (i32.const 128)))
(func $blue  (param $iter i32) (result i32) (call $8_bit_clamp (local.get $iter) (i32.const 356)))
(func $colour
      (param $iter i32)
      (result i32)

  (local $iter4 i32)
  (local.set $iter4 (i32.shl (local.get $iter) (i32.const 2)))

  ;; Little-endian processors require the colour component values in ABGR order, not RGBA
  (i32.or
    (i32.or
      (i32.const 0xFF000000)   ;; Fully opaque
      (i32.shl (call $blue (local.get $iter4)) (i32.const 16))
    )
    (i32.or
      (i32.shl (call $green (local.get $iter4)) (i32.const 8))
      (call $red (local.get $iter4))
    )
  )
)
(func (export "gen_palette")
      (param $max_iters i32)

  (local $idx i32)

  (loop $next
    (if (i32.gt_u (local.get $max_iters) (local.get $idx))
      (then
        (i32.store
           (i32.add (global.get $PALETTE_OFFSET) (i32.shl (local.get $idx) (i32.const 2)))
           (call $colour (local.get $idx))
        )

        (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
        (br $next)
      )
    )
  )
)

;; MANDELBROT:
  (func $dup (param i32) (result i32 i32)
    local.get 0
    local.get 0
  )

  (func $center (param $a i32) (param $size i32) (result i32)
    ;; a
    local.get 0
    local.get 1
    i32.const 2
    i32.div_u
    i32.sub
  )

  ;; check for early bailout
  ;; if (x,y) in cardioid or bulb
  (func $shouldBailoutEarly (param $x v128) (param $y v128) (result v128)
    (local $ySquared v128)
    (local $xMinusOneFourth v128)
    (local $xPlusOne v128)
    (local $q v128)

    local.get $y
    local.get $y
    f32x4.mul
    local.tee $ySquared

    local.get $x
    v128.const f32x4 0.25 0.25 0.25 0.25
    f32x4.sub
    local.tee $xMinusOneFourth
    local.get $xMinusOneFourth
    f32x4.mul

    f32x4.add

    local.tee $q
    local.get $xMinusOneFourth
    f32x4.add
    local.get $q
    f32x4.mul

    v128.const f32x4 0.25 0.25 0.25 0.25
    local.get $ySquared
    f32x4.mul

    f32x4.lt

    local.get $x
    v128.const f32x4 1.0 1.0 1.0 1.0
    f32x4.add
    local.tee $xPlusOne
    local.get $xPlusOne
    f32x4.mul
    local.get $ySquared
    f32x4.add
    v128.const f32x4 0.0625 0.0625 0.0625 0.0625
    f32x4.lt

    v128.or
  )
  ;; f_c(z) = z*z + c
  (func $mandelbrotPixelShader (param $_x v128) (param $_y v128) (param $bailoutx4 v128) (param $paletteOffsetx4 v128) (result v128)
    (local $ix4 v128)
    (local $shouldIncreaseI v128)

    (local $x v128)
    (local $y v128)

    (local $zx v128)
    (local $zy v128)
    (local $zxSquared v128)
    (local $zySquared v128)

    (local $addressesx4 v128)
    (local $colors v128)
    (local $color0 i32)
    (local $color1 i32)
    (local $color2 i32)
    (local $color3 i32)

    local.get $_x
    v128.const f32x4 2.74 2.74 2.74 2.74
    f32x4.mul
    v128.const f32x4 2.1 2.1 2.1 2.1
    f32x4.sub
    local.tee $x

    local.get $_y
    v128.const f32x4 2.5 2.5 2.5 2.5
    f32x4.mul
    v128.const f32x4 1.25 1.25 1.25 1.25
    f32x4.sub
    local.tee $y

    call $shouldBailoutEarly
    v128.any_true
    if
      v128.const i32x4 1000 1000 1000 1000
      local.set $ix4
    else
      ;; escape time loop
      loop $loop
        local.get $ix4
        v128.const i32x4 1000 1000 1000 1000
        i32x4.lt_u

        local.get $zx
        local.get $zx
        f32x4.mul
        local.tee $zxSquared
        local.get $zy
        local.get $zy
        f32x4.mul
        local.tee $zySquared
        f32x4.add
        local.get $bailoutx4
        f32x4.lt

        v128.and
        local.tee $shouldIncreaseI
        v128.any_true
        if
          ;; y := 2*x*y + y0
          local.get $zx
          local.get $zx
          f32x4.add
          local.get $zy
          f32x4.mul
          local.get $y
          f32x4.add
          local.set $zy

          ;; xtemp := x^2 - y^2 + x0
          local.get $zxSquared
          local.get $zySquared
          f32x4.sub
          local.get $x
          f32x4.add
          local.set $zx

          local.get $ix4
          v128.const i32x4 1 1 1 1
          local.get $shouldIncreaseI
          v128.and
          i32x4.add
          local.set $ix4
          br $loop
        end
      end
    end

    ;; TODO: is there a better way to do that?

    ;; computing the addresses of the color
    local.get $paletteOffsetx4
    local.get $ix4
    i32.const 2
    i32x4.shl
    i32x4.add

    ;; loading the 4 colors on the stack
    local.tee $addressesx4
    i32x4.extract_lane 0
    i32.load
    local.set $color0

    local.get $addressesx4
    i32x4.extract_lane 1
    i32.load
    local.set $color1

    local.get $addressesx4
    i32x4.extract_lane 2
    i32.load
    local.set $color2

    local.get $addressesx4
    i32x4.extract_lane 3
    i32.load
    local.set $color3

    ;; replacing lanes in $colors
    local.get $colors
    local.get $color0
    i32x4.replace_lane 0
    local.get $color1
    i32x4.replace_lane 1
    local.get $color2
    i32x4.replace_lane 2
    local.get $color3
    i32x4.replace_lane 3
  )

  (func (export "mandelbrot") (param $w i32) (param $h i32)
    (local $w_i i32)
    (local $h_i i32)

    (local $w_f f32)
    (local $h_f f32)

    (local $w_fx4 v128)
    (local $h_fx4 v128)

    (local $bailoutx4 v128)
    (local $paletteOffsetx4 v128)

    global.get $BAILOUT
    f32x4.splat
    local.set $bailoutx4

    global.get $PALETTE_OFFSET
    i32x4.splat
    local.set $paletteOffsetx4

    local.get $w
    f32.convert_i32_u
    local.tee $w_f
    f32x4.splat
    local.set $w_fx4

    local.get $h
    f32.convert_i32_u
    local.tee $h_f
    f32x4.splat
    local.set $h_fx4

    loop $loop_h
      ;; i < h
      local.get $h_i
      local.get $h
      i32.lt_u
      if
        loop $loop_w
          ;; i < w
          local.get $w_i
          local.get $w
          i32.lt_u
          if
            ;; dest address
            local.get $h_i
            local.get $w
            i32.mul
            local.get $w_i
            i32.add

            i32.const 2
            i32.shl

            ;; pixel color
            local.get $w_i
            i32x4.splat
            v128.const i32x4 0 1 2 3
            i32x4.add
            f32x4.convert_i32x4_u
            local.get $w_fx4
            f32x4.div

            local.get $h_i
            f32.convert_i32_u
            f32x4.splat
            local.get $h_fx4
            f32x4.div

            local.get $bailoutx4
            local.get $paletteOffsetx4
            call $mandelbrotPixelShader

            v128.store

            ;; w_i += 4
            local.get $w_i
            i32.const 4
            i32.add
            local.set $w_i
            br $loop_w
          end
        end
        i32.const 0
        local.set $w_i

        ;; h_i += 1
        local.get $h_i
        i32.const 1
        i32.add
        local.set $h_i
        br $loop_h
      end
    end
  )
)
