enum IteratorExample {
    static func run() -> Bool {
        var iterator = [10, 20, 30].makeIterator()
        let seen = [iterator.next(), iterator.next(), iterator.next()].compactMap { $0 }
        return seen == [10, 20, 30] && iterator.next() == nil
    }
}
