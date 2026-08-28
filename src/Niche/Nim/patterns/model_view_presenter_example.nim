proc run*(): bool =
  var count = 0
  var text = ""
  proc present() =
    inc count
    text = "count=" & $count
  present()
  count == 1 and text == "count=1"
