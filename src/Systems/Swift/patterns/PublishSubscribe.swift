enum PublishSubscribeExample {
    static func run() -> Bool {
        let subscribers: [(Int) -> String] = [
            { "warehouse:\($0)" },
            { "analytics:\($0)" }
        ]
        return subscribers.map { $0(51) }.joined(separator: ">") == "warehouse:51>analytics:51"
    }
}
