proc run*(): bool =
  proc remote(sku: string): int =
    if sku == "sku-1":
      7
    else:
      0
  proc proxy(sku: string): int =
    remote(sku)
  proxy("sku-1") == 7
