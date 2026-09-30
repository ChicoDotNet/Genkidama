def analytics(message: Int) -> Int:
    return message + 1


def warehouse(message: Int) -> Int:
    return message + 10


def publish(message: Int, first: def(Int) thin -> Int, second: def(Int) thin -> Int) -> Int:
    return first(message) + second(message)


def run() -> Bool:
    return publish(5, analytics, warehouse) == 21
