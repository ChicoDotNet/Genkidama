import std/tables
proc run*(): bool =
  type Service = proc(key: string): string
  var services = initTable[string, Service]()
  services["inventory"] = proc(key: string): string = "inventory:" & key & "=7"
  services["customer"] = proc(key: string): string = "customer:" & key & "=active"
  services["inventory"]("sku-1") == "inventory:sku-1=7" and services["customer"]("17") == "customer:17=active"
