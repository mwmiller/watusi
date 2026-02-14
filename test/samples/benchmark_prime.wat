(module
  ;; Function to check if a number is prime
  (func $is_prime (param $n i32) (result i32)
    (local $i i32)
    (local $sqrt_n i32)
    
    (if (i32.lt_s (local.get $n) (i32.const 2))
      (then (return (i32.const 0)))
    )
    (if (i32.eq (local.get $n) (i32.const 2))
      (then (return (i32.const 1)))
    )
    (if (i32.eq (i32.rem_u (local.get $n) (i32.const 2)) (i32.const 0))
      (then (return (i32.const 0)))
    )
    
    ;; Calculate approximate square root
    (local.set $sqrt_n (i32.trunc_f32_s (f32.sqrt (f32.convert_i32_s (local.get $n)))))
    
    ;; Check odd divisors from 3 to sqrt(n)
    (local.set $i (i32.const 3))
    (loop $check_loop
      (if (i32.le_s (local.get $i) (local.get $sqrt_n))
        (then
          (if (i32.eq (i32.rem_u (local.get $n) (local.get $i)) (i32.const 0))
            (then (return (i32.const 0)))
          )
          (local.set $i (i32.add (local.get $i) (i32.const 2)))
          (br $check_loop)
        )
      )
    )
    (return (i32.const 1))
  )
  
  (func $nth_prime (param $n i32) (result i32)
    (local $count i32)
    (local $candidate i32)
    
    (local.set $count (i32.const 0))
    (local.set $candidate (i32.const 2))
    
    (loop $find_loop
      (if (call $is_prime (local.get $candidate))
        (then
          (local.set $count (i32.add (local.get $count) (i32.const 1)))
          (if (i32.eq (local.get $count) (local.get $n))
            (then (return (local.get $candidate)))
          )
        )
      )
      (local.set $candidate (i32.add (local.get $candidate) (i32.const 1)))
      (br $find_loop)
    )
    (return (i32.const 0))
  )
  
  (func (export "nth_prime") (param i32) (result i32)
    (return (call $nth_prime (local.get 0)))
  )
)