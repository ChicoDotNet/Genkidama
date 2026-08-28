enum MicrokernelExample {
    static func run() -> Bool {
        let plugins: [String: (Int) -> Int] = [
            "double": { $0 * 2 },
            "square": { $0 * $0 }
        ]
        return plugins["double"]!(4) == 8 && plugins["square"]!(4) == 16
    }
}
