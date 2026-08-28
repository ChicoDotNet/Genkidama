import std/[sequtils, strutils]
proc run*(): bool =
  let observers = [proc(id: int): string = "audit:" & $id, proc(id: int): string = "dashboard:" & $id]
  observers.mapIt(it(42)).join(">") == "audit:42>dashboard:42"
