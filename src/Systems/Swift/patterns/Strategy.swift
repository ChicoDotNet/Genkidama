enum StrategyExample {
    static func run() -> Bool {
        let price: (Int, (Int) -> Int) -> Int = { value, strategy in strategy(value) }
        return price(100, { $0 }) == 100 && price(100, { $0 * 80 / 100 }) == 80
    }
}
