enum MicroservicesExample {
    static func run() -> Bool {
        var stock = 7
        let reserve: (Int) -> Bool = { quantity in
            if quantity > stock { return false }
            stock -= quantity
            return true
        }
        let place: (Int) -> String = { reserve($0) ? "confirmed" : "rejected" }
        return place(2) == "confirmed" && stock == 5
    }
}
