(module
    (type $t (func))
    (table 0 funcref)
    (func $type-ref-vs-funcref (result (ref null $t))
      (return_call_indirect (result funcref) (i32.const 0))
    )
  )
