struct ActiveObject:
    var pending: Int
    var result: Int

    def __init__(out self):
        self.pending = 0
        self.result = 0

    def enqueue(mut self, value: Int):
        self.pending += value

    def drain(mut self):
        self.result += self.pending
        self.pending = 0


def run() -> Bool:
    var active = ActiveObject()
    active.enqueue(2)
    active.enqueue(3)
    active.drain()
    return active.result == 5 and active.pending == 0
