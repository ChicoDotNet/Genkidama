class Registry:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance.count = 0
        return cls._instance


first = Registry()
second = Registry()
first.count += 1
print("same=" + ("true" if first is second else "false"))
print("count=" + str(second.count))
