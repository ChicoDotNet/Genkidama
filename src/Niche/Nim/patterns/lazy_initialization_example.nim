proc run*(): bool =
  var builds = 0
  var cache = ""
  proc getValue(): string =
    if cache.len == 0:
      inc builds
      cache = "ready"
    cache
  getValue() == "ready" and getValue() == "ready" and builds == 1
