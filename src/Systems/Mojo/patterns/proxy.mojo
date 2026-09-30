def remote_fetch(key: Int) -> Int:
    return key * 10


struct CachedProxy:
    var cached: Int
    var remote_calls: Int

    def __init__(out self):
        self.cached = -1
        self.remote_calls = 0

    def fetch(mut self, key: Int) -> Int:
        if self.cached < 0:
            self.cached = remote_fetch(key)
            self.remote_calls += 1
        return self.cached


def run() -> Bool:
    var proxy = CachedProxy()
    var first = proxy.fetch(4)
    var second = proxy.fetch(4)
    return first == 40 and second == 40 and proxy.remote_calls == 1
