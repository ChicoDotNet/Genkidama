enum DistributedProxyExample {
    static func run() -> Bool {
        let remote: (String) -> Int = { $0 == "sku-1" ? 7 : 0 }
        let proxy: (String) -> Int = { remote($0) }
        return proxy("sku-1") == 7
    }
}
