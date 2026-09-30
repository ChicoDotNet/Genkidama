def regular_step(value: Int) -> Int:
    return value * 2


def premium_step(value: Int) -> Int:
    return value * 3


def workflow(value: Int, hook: def(Int) thin -> Int) -> Int:
    var prepared = value + 1
    var varied = hook(prepared)
    return varied + 1


def run() -> Bool:
    return workflow(2, regular_step) == 7 and workflow(2, premium_step) == 10
