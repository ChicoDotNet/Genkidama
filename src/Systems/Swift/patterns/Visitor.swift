enum VisitorExample {
    enum Shape {
        case circle(Double)
        case rect(Double, Double)
    }

    static func area(_ shape: Shape) -> Double {
        switch shape {
        case .circle(let radius): return Double.pi * radius * radius
        case .rect(let width, let height): return width * height
        }
    }

    static func run() -> Bool {
        let total = [Shape.circle(2), .rect(3, 4)].map(area).reduce(0, +)
        return abs(total - (4 * Double.pi + 12)) < 1e-9
    }
}
