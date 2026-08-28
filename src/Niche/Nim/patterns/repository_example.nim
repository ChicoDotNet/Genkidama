import std/tables
proc run*(): bool =
  let rows = {1: "Ada", 2: "Grace"}.toTable
  rows[2] == "Grace"
