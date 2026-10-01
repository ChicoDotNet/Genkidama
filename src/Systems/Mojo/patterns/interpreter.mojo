def literal(value: Int) -> Int:
    return value


def add(left: Int, right: Int) -> Int:
    return left + right


def multiply(left: Int, right: Int) -> Int:
    return left * right


def run() -> Bool:
    var expression = multiply(add(literal(2), literal(3)), literal(4))
    return expression == 20
