(module
  ;; Extended constant expressions allow e.g. i32.add in global initializers
  (global $g i32 (i32.add (i32.const 40) (i32.const 2)))
  (func (export "get_global") (result i32)
    global.get $g
  )
) 