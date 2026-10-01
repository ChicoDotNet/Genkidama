def real_logger(value: Int) -> Int:
    return value


def null_logger(value: Int) -> Int:
    return 0


def service(value: Int, logger: def(Int) thin -> Int) -> Int:
    return value * 10 + logger(value)


def run() -> Bool:
    return service(3, real_logger) == 33 and service(3, null_logger) == 30
