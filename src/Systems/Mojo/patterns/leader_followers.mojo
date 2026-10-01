struct WorkerPool:
    var leader: Int
    var workers: Int

    def __init__(out self, workers: Int):
        self.leader = 0
        self.workers = workers

    def handle(mut self) -> Int:
        var current = self.leader
        self.leader = (self.leader + 1) % self.workers
        return current


def run() -> Bool:
    var pool = WorkerPool(3)
    return pool.handle() == 0 and pool.handle() == 1 and pool.handle() == 2 and pool.handle() == 0
