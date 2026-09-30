def abstraction(value: Int) -> Int:
    return value * 2


def presentation(value: Int) -> Int:
    return value + 1


def control(input_value: Int) -> Int:
    return presentation(abstraction(input_value))


def run() -> Bool:
    return control(4) == 9
