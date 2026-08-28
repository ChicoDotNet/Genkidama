enum ServiceLocatorExample {
    static func run() -> Bool {
        let services: [String: (String) -> String] = [
            "email": { "email>\($0)" },
            "audit": { "audit>\($0)" }
        ]
        return services["email"]!("a@example.test") == "email>a@example.test"
            && services["audit"]!("created") == "audit>created"
    }
}
