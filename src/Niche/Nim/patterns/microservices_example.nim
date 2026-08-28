proc run*(): bool =
  var stock = 7
  proc reserve(quantity: int): bool =
    if quantity > stock: return false
    stock -= quantity; true
  proc place(quantity: int): string = if reserve(quantity): "confirmed" else: "rejected"
  place(2) == "confirmed" and stock == 5
