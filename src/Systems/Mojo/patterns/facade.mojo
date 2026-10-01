def authenticate(user: Int) -> Int:
    return user + 1


def reserve(item: Int) -> Int:
    return item + 2


def charge(amount: Int) -> Int:
    return amount + 3


def checkout(user: Int, item: Int, amount: Int) -> Int:
    return authenticate(user) + reserve(item) + charge(amount)


def run() -> Bool:
    return checkout(1, 2, 3) == 12
