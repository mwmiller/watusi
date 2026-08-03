(module
    (memory 1)
    (func
      i32.const 0
      i32.load offset=0xFFFF_FFFF_FFFF_FFFF align=0x8000_0000_0000_0000
      drop
    )
  )
