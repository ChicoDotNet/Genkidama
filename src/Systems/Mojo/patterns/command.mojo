def increment(value: Int) -> Int:
    return value + 1


def double(value: Int) -> Int:
    return value * 2


def invoke(value: Int, command: def(Int) thin -> Int) -> Int:
    return command(value)


def run() -> Bool:
    var value = invoke(3, increment)
    value = invoke(value, double)
    return value == 8
