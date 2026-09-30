from std.utils.lock import BlockingSpinLock


struct CounterMonitor:
    var _lock: BlockingSpinLock
    var _value: Int

    def __init__(out self):
        self._lock = BlockingSpinLock()
        self._value = 0

    def increment(mut self, amount: Int = 1):
        self._lock.lock(1)
        self._value += amount
        _ = self._lock.unlock(1)

    def current(mut self) -> Int:
        self._lock.lock(1)
        var value = self._value
        _ = self._lock.unlock(1)
        return value
