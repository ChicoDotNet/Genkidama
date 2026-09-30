def audit(value: Int) -> Int:
    return value + 1


def metrics(value: Int) -> Int:
    return value + 10


def notify(value: Int, first: def(Int) thin -> Int, second: def(Int) thin -> Int) -> Int:
    return first(value) + second(value)


def run() -> Bool:
    return notify(5, audit, metrics) == 21
