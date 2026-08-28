proc run*(): bool =
  var pool = @[1, 2]
  let value = pool.pop()
  pool.add(value)
  pool.len == 2 and value in pool
