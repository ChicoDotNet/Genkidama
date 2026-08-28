enum BrokerExample {
    static func run() -> Bool {
        let services: [String: (String) -> String] = [
            "inventory": { "inventory:\($0)=7" },
            "customer": { "customer:\($0)=active" }
        ]
        return services["inventory"]!("sku-1") == "inventory:sku-1=7"
            && services["customer"]!("17") == "customer:17=active"
    }
}
