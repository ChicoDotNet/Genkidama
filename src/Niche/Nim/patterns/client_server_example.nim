proc run*(): bool =
  proc server(key: string): tuple[status: int, body: string] =
    if key == "sku-1":
      (status: 200, body: "stock=7")
    else:
      (status: 404, body: "missing")
  server("sku-1") == (status: 200, body: "stock=7")
