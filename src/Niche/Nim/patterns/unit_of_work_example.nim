proc run*(): bool =
  var store: seq[int] = @[]
  var pending = @[2, 3]
  store.add(pending)
  pending.setLen(0)
  store == @[2, 3] and pending.len == 0
