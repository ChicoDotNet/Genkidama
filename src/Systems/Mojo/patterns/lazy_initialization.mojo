struct LazyValue:
    var initialized: Bool
    var value: Int
    var creations: Int

    def __init__(out self):
        self.initialized = False
        self.value = 0
        self.creations = 0

    def get(mut self) -> Int:
        if not self.initialized:
            self.value = 42
            self.creations += 1
            self.initialized = True
        return self.value


def run() -> Bool:
    var lazy = LazyValue()
    return lazy.get() == 42 and lazy.get() == 42 and lazy.creations == 1
