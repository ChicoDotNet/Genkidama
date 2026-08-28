proc run*(): bool =
  proc server(key: string): tuple[status: int, body: string] = if key == "sku-1": (200, "stock=7") else: (404, "missing")
  server("sku-1") == (status: 200, body: "stock=7")
