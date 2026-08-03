(module
    (memory 1)
    (func
      i32.const 0
      i32.load offset=0xFFFF_FFFF_FFFF_FFFF
      drop
    )
  )
