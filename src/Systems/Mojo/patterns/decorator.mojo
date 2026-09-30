def base_message(value: Int) -> Int:
    return value


def encrypted(inner: Int) -> Int:
    return inner + 10


def audited(inner: Int) -> Int:
    return inner + 100


def run() -> Bool:
    var base = base_message(1)
    var decorated = audited(encrypted(base))
    return base == 1 and decorated == 111
