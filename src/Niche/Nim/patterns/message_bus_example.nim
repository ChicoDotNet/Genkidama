import std/[sequtils, strutils]
proc run*(): bool =
  let handlers = [proc(topic: string, id: int): string = "audit:" & topic & ":" & $id, proc(topic: string, id: int): string = "billing:" & topic & ":" & $id]
  handlers.mapIt(it("order-created", 42)).join(">") == "audit:order-created:42>billing:order-created:42"
