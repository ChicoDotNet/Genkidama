enum UnitOfWorkExample {
    static func run() -> Bool {
        var store: [Int] = []
        var pending = [2, 3]
        store.append(contentsOf: pending)
        pending.removeAll()
        return store == [2, 3] && pending.isEmpty
    }
}
