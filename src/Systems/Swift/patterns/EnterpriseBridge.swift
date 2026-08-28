enum EnterpriseBridgeExample {
    static func run() -> Bool {
        let send: (String, String, String) -> String = { transport, kind, message in
            "\(transport)>\(kind):\(message)"
        }
        return send("kafka", "ALERT", "disk") == "kafka>ALERT:disk"
            && send("queue", "REMINDER", "backup") == "queue>REMINDER:backup"
    }
}
