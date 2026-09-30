struct Model:
    var value: Int

    def __init__(out self, value: Int):
        self.value = value


def controller_increment(mut model: Model):
    model.value += 1


def view(model: Model) -> Int:
    return model.value * 10


def run() -> Bool:
    var model = Model(4)
    controller_increment(model)
    return model.value == 5 and view(model) == 50
