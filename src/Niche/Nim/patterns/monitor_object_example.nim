import std/locks
proc run*(): bool =
  var gate: Lock
  initLock(gate)
  var value = 0
  withLock gate: value += 2
  withLock gate: value += 3
  deinitLock(gate)
  value == 5
