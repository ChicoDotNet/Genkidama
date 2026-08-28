import std/tables
proc run*(): bool =
  type Service = proc(value: string): string
  var services = initTable[string, Service]()
  services["email"] = proc(value: string): string = "email>" & value
  services["audit"] = proc(value: string): string = "audit>" & value
  services["email"]("a@example.test") == "email>a@example.test" and services["audit"]("created") == "audit>created"
