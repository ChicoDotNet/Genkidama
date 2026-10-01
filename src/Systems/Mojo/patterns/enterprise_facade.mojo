def identity_system(user: Int) -> Int:
    return user + 1


def inventory_system(item: Int) -> Int:
    return item + 2


def billing_system(amount: Int) -> Int:
    return amount + 3


def enterprise_checkout(user: Int, item: Int, amount: Int) -> Int:
    return identity_system(user) + inventory_system(item) + billing_system(amount)


def run() -> Bool:
    return enterprise_checkout(1, 2, 3) == 12
