struct Pool:
    var available: Int

    def __init__(out self, size: Int):
        self.available = size

    def acquire(mut self) -> Bool:
        if self.available == 0:
            return False
        self.available -= 1
        return True

    def release(mut self):
        self.available += 1


def run() -> Bool:
    var pool = Pool(1)
    var first = pool.acquire()
    var second = pool.acquire()
    pool.release()
    var third = pool.acquire()
    return first and not second and third
