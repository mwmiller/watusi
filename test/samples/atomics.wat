(module
  (memory 1 1 shared)
  (func (export "atomic_add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.atomic.rmw.add)
  (func (export "notify") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    memory.atomic.notify)
  (func (export "cmpxchg") (param i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    i32.atomic.rmw.cmpxchg offset=4 align=4)
)
