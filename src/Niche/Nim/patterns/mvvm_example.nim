proc run*(): bool =
  var amount = 10
  proc text(): string = "$" & $amount & ".00"
  let before = text(); amount += 5
  before == "$10.00" and text() == "$15.00"
