enum LazyInitializationExample {
    static func run() -> Bool {
        var builds = 0
        var value: String?
        func get() -> String {
            if value == nil {
                builds += 1
                value = "ready"
            }
            return value!
        }
        let first = get()
        let second = get()
        return first == "ready" && second == "ready" && builds == 1
    }
}
