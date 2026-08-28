enum InterpreterExample {
    indirect enum Expr {
        case lit(Int)
        case add(Expr, Expr)
        case mul(Expr, Expr)
    }

    static func evaluate(_ expression: Expr) -> Int {
        switch expression {
        case .lit(let value): return value
        case .add(let left, let right): return evaluate(left) + evaluate(right)
        case .mul(let left, let right): return evaluate(left) * evaluate(right)
        }
    }

    static func run() -> Bool {
        evaluate(.add(.lit(7), .mul(.lit(3), .lit(4)))) == 19
    }
}
