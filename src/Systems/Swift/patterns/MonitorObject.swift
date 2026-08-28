import Foundation

enum MonitorObjectExample {
    final class Counter {
        private let lock = NSLock()
        private var value = 0

        func add(_ amount: Int) {
            lock.lock()
            defer { lock.unlock() }
            value += amount
        }

        func get() -> Int {
            lock.lock()
            defer { lock.unlock() }
            return value
        }
    }

    static func run() -> Bool {
        let counter = Counter()
        counter.add(2)
        counter.add(3)
        return counter.get() == 5
    }
}
