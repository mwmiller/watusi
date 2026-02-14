(module
  ;; Test multiple memories support
  (memory $mem1 1)
  (memory $mem2 1)
  (export "mem1" (memory $mem1))
  (export "mem2" (memory $mem2))
)