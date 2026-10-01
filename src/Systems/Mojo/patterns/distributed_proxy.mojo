def remote_service(key: Int) -> Int:
    return key * 100


struct DistributedProxy:
    var cached_key: Int
    var cached_value: Int
    var calls: Int

    def __init__(out self):
        self.cached_key = -1
        self.cached_value = 0
        self.calls = 0

    def fetch(mut self, key: Int) -> Int:
        if self.cached_key != key:
            self.cached_key = key
            self.cached_value = remote_service(key)
            self.calls += 1
        return self.cached_value


def run() -> Bool:
    var proxy = DistributedProxy()
    return proxy.fetch(3) == 300 and proxy.fetch(3) == 300 and proxy.calls == 1
