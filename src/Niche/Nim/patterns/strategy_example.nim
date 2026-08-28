proc run*(): bool =
  proc price(value: int, strategy: proc(x: int): int): int = strategy(value)
  price(100, proc(x: int): int = x) == 100 and price(100, proc(x: int): int = x * 80 div 100) == 80
