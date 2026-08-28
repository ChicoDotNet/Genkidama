import std/[sequtils, strutils]
proc run*(): bool =
  let subscribers = [proc(id: int): string = "warehouse:" & $id, proc(id: int): string = "analytics:" & $id]
  subscribers.mapIt(it(51)).join(">") == "warehouse:51>analytics:51"
