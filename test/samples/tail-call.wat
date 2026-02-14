(module
  ;; Test tail call support
  (func $helper (param i32) (result i32)
    local.get 0
  )
  (func (export "test") (param i32) (result i32)
    local.get 0
    return_call $helper
  )
) 