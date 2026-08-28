proc run*(): bool =
  var seen: seq[int] = @[]
  for value in [10, 20, 30]: seen.add(value)
  seen == @[10, 20, 30]
