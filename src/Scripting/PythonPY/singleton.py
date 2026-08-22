class ProcessRegistry:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance.count = 0
        return cls._instance

    def increment(self):
        self.count += 1


first = ProcessRegistry()
second = ProcessRegistry()
first.increment()
print(f"same={str(first is second).lower()}")
print(f"count={second.count}")
