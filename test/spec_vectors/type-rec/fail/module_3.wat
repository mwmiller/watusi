(module
    (rec (type (func)) (type $ft (func)))
    (func $f)  ;; the implicit type of $f is not $ft
    (global (ref $ft) (ref.func $f))
  )
