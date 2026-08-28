import std/tables
proc run*(): bool =
  type Plugin = proc(x: int): int
  var plugins = initTable[string, Plugin]()
  plugins["double"] = proc(x: int): int = x * 2
  plugins["square"] = proc(x: int): int = x * x
  plugins["double"](4) == 8 and plugins["square"](4) == 16
