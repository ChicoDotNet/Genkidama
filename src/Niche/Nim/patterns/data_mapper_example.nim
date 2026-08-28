import std/strformat
proc run*(): bool =
  let id = 8
  let name = "Grace"
  let key = &"person:{id}"
  key == "person:8" and name == "Grace"
