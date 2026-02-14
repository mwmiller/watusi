(module
  (memory (import "env" "memory") 1 1 shared)

  (func (export "store")
    (i32.atomic.store (i32.const 0) (i32.const 123))
  )

  (func (export "load") (result i32)
    (i32.atomic.load (i32.const 0))
  )
)
