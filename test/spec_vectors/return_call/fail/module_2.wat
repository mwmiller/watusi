(module
    (type $t (func))
    (func $type-ref-vs-funcref (result (ref null $t)) (return_call 1))
    (func (result funcref) (unreachable))
  )
