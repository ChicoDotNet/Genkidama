module InterpreterExample

type Expr =
    | Lit of int
    | Add of Expr * Expr
    | Mul of Expr * Expr

let rec private eval = function
    | Lit value -> value
    | Add (left, right) -> eval left + eval right
    | Mul (left, right) -> eval left * eval right

let run () =
    eval (Add (Lit 7, Mul (Lit 3, Lit 4))) = 19
