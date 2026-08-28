import std/tables
proc run*(): bool =
  var table = initTable[int, string]()
  table[7] = "Ada"
  table[7] == "Ada"
