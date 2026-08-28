proc run*(): bool =
  let commands = [proc(x: int): int = x + 50, proc(x: int): int = x - 20]
  var balance = 100
  for command in commands: balance = command(balance)
  balance == 130 and commands[1](150) == 130
