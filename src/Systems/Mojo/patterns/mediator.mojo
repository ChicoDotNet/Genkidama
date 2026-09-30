struct Mediator:
    var total: Int

    def __init__(out self):
        self.total = 0

    def send(mut self, colleague: Int, value: Int):
        if colleague == 1:
            self.total += value
        else:
            self.total += value * 10


def run() -> Bool:
    var mediator = Mediator()
    mediator.send(1, 2)
    mediator.send(2, 3)
    return mediator.total == 32
