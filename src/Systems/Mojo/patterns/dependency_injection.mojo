def production_dependency(value: Int) -> Int:
    return value * 10


def fake_dependency(value: Int) -> Int:
    return value + 1


struct Service:
    var dependency: def(Int) thin -> Int

    def __init__(out self, dependency: def(Int) thin -> Int):
        self.dependency = dependency

    def execute(self, value: Int) -> Int:
        return self.dependency(value)


def run() -> Bool:
    var production = Service(production_dependency)
    var fake = Service(fake_dependency)
    return production.execute(3) == 30 and fake.execute(3) == 4
