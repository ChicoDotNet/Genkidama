proc run*(): bool =
  proc nullLog(_: string): string = ""
  proc realLog(message: string): string = "log:" & message
  nullLog("x") == "" and realLog("x") == "log:x"
