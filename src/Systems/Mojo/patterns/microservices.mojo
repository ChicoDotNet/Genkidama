def inventory_service(item: Int) -> Int:
    if item == 42:
        return 1
    return 0


def pricing_service(item: Int) -> Int:
    return item * 2


def order_service(item: Int) -> Int:
    if inventory_service(item) == 0:
        return -1
    return pricing_service(item)


def run() -> Bool:
    return order_service(42) == 84 and order_service(7) == -1
