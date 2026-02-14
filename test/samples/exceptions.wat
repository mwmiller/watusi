(module
  (tag $e) ;; define a new exception tag (no payload)

  (func $thrower
    (throw $e)
  )

  (func (export "test_exception")
    (try
      (do
        (call $thrower)
      )
      (catch $e
        ;; handle exception here
      )
    )
  )
)