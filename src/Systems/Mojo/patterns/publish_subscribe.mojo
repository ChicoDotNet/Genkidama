def subscriber_a(value: Int) -> Int:
    return value + 1


def subscriber_b(value: Int) -> Int:
    return value + 2


def publish(value: Int, first: def(Int) thin -> Int, second: def(Int) thin -> Int) -> Int:
    return first(value) + second(value)


def run() -> Bool:
    return publish(5, subscriber_a, subscriber_b) == 13
