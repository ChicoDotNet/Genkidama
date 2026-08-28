enum ObserverExample {
    static func run() -> Bool {
        let observers: [(Int) -> String] = [
            { "audit:\($0)" },
            { "dashboard:\($0)" }
        ]
        return observers.map { $0(42) }.joined(separator: ">") == "audit:42>dashboard:42"
    }
}
