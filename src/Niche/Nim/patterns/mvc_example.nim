proc run*(): bool =
  var count = 0
  proc view(): string = "count=" & $count
  let before = view(); inc count
  before == "count=0" and view() == "count=1"
