enum ObjectPoolExample {
    static func run() -> Bool {
        var pool = [1, 2]
        let borrowed = pool.removeLast()
        pool.append(borrowed)
        return pool.count == 2 && pool.contains(borrowed)
    }
}
