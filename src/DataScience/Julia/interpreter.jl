abstract type Expr end

struct Number <: Expr
    value::Int
end

struct Add <: Expr
    left::Expr
    right::Expr
end

interpret(expr::Number) = expr.value
interpret(expr::Add) = interpret(expr.left) + interpret(expr.right)

expr = Add(Number(2), Add(Number(3), Number(4)))
value = interpret(expr)
value == 9 || error("expected 9, got $value")
println("value=$value")
