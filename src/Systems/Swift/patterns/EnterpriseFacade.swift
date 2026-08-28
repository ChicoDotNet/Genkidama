enum EnterpriseFacadeExample {
    static func run() -> Bool {
        let crm: (Int) -> String = { "crm:create:\($0)" }
        let billing: (Int) -> String = { "billing:open:\($0)" }
        return "\(crm(77))>\(billing(77))" == "crm:create:77>billing:open:77"
    }
}
