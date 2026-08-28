enum MVVMExample {
    static func run() -> Bool {
        var amount = 10
        let text = { "$\(amount).00" }
        let before = text()
        amount += 5
        return before == "$10.00" && text() == "$15.00"
    }
}
