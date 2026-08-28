enum ActiveRecordExample {
    static func run() -> Bool {
        var table: [Int: String] = [:]
        table[7] = "Ada"
        return table[7] == "Ada"
    }
}
