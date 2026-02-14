(module
  ;; Test relaxed SIMD support
  (func (export "test")
    (drop
      (f32x4.relaxed_min
        (v128.const f32x4 1.0 2.0 3.0 4.0)
        (v128.const f32x4 4.0 3.0 2.0 1.0)
      )
    )
  )
)