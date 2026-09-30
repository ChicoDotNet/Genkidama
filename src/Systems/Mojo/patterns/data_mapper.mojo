struct Domain:
    var id: Int
    var value: Int

    def __init__(out self, id: Int, value: Int):
        self.id = id
        self.value = value


def from_row(id: Int, value: Int) -> Domain:
    return Domain(id, value)


def to_row(domain: Domain) -> Int:
    return domain.id * 100 + domain.value


def run() -> Bool:
    var domain = from_row(4, 7)
    return domain.value == 7 and to_row(domain) == 407
