struct Prototype:
    var value: Int

    def __init__(out self, value: Int):
        self.value = value

    def clone(self) -> Prototype:
        return Prototype(self.value)

    def set_value(mut self, value: Int):
        self.value = value


def run() -> Bool:
    var original = Prototype(4)
    var copy = original.clone()
    copy.set_value(9)
    return original.value == 4 and copy.value == 9
