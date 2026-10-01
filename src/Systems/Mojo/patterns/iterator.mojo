struct NumberIterator:
    var current: Int
    var stop: Int

    def __init__(out self, stop: Int):
        self.current = 0
        self.stop = stop

    def has_next(self) -> Bool:
        return self.current < self.stop

    def next(mut self) -> Int:
        self.current += 1
        return self.current


def run() -> Bool:
    var iterator = NumberIterator(3)
    var total = 0
    while iterator.has_next():
        total += iterator.next()
    return total == 6
