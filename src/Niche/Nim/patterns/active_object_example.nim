proc run*(): bool =
  var value = 0
  let queue = [proc() = value += 3, proc() = value *= 4]
  let before = value
  for command in queue: command()
  before == 0 and value == 12
