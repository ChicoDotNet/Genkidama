struct UnitOfWork:
    var pending_total: Int
    var pending_count: Int

    def __init__(out self):
        self.pending_total = 0
        self.pending_count = 0

    def register(mut self, value: Int):
        self.pending_total += value
        self.pending_count += 1

    def commit(mut self) -> Int:
        var total = self.pending_total
        self.pending_total = 0
        self.pending_count = 0
        return total


def run() -> Bool:
    var work = UnitOfWork()
    work.register(2)
    work.register(3)
    var committed = work.commit()
    return committed == 5 and work.pending_count == 0
