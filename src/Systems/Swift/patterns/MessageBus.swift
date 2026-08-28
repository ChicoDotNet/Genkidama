enum MessageBusExample {
    static func run() -> Bool {
        let handlers: [(String, Int) -> String] = [
            { "audit:\($0):\($1)" },
            { "billing:\($0):\($1)" }
        ]
        return handlers.map { $0("order-created", 42) }.joined(separator: ">")
            == "audit:order-created:42>billing:order-created:42"
    }
}
