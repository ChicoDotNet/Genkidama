enum EnterpriseAdapterExample {
    static func run() -> Bool {
        let legacy = (code: 17, cents: 1250)
        let canonical = (id: legacy.code, amount: Double(legacy.cents) / 100)
        return canonical.id == 17 && canonical.amount == 12.5
    }
}
