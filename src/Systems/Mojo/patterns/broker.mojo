def inventory_service(value: Int) -> Int:
    return value + 10


def billing_service(value: Int) -> Int:
    return value + 20


def broker_route(service: Int, value: Int) -> Int:
    if service == 1:
        return inventory_service(value)
    return billing_service(value)


def run() -> Bool:
    return broker_route(1, 5) == 15 and broker_route(2, 5) == 25
