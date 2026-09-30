def double(value: Int) -> Int:
    return value * 2


def square(value: Int) -> Int:
    return value * value


def apply_strategy(value: Int, strategy: def(Int) thin -> Int) -> Int:
    return strategy(value)
