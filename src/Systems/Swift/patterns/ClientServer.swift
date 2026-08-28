enum ClientServerExample {
    static func run() -> Bool {
        let server: (String) -> (Int, String) = {
            $0 == "sku-1" ? (200, "stock=7") : (404, "missing")
        }
        let response = server("sku-1")
        return response.0 == 200 && response.1 == "stock=7"
    }
}
