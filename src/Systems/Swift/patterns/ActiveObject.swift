enum ActiveObjectExample {
    static func run() -> Bool {
        var value = 0
        let queue: [() -> Void] = [{ value += 3 }, { value *= 4 }]
        let before = value
        queue.forEach { $0() }
        return before == 0 && value == 12
    }
}
