type
  ExprKind = enum
    numberExpr, addExpr
  Expr = ref object
    case kind: ExprKind
    of numberExpr:
      value: int
    of addExpr:
      left, right: Expr

proc number(value: int): Expr =
  Expr(kind: numberExpr, value: value)

proc add(left, right: Expr): Expr =
  Expr(kind: addExpr, left: left, right: right)

proc interpret(expression: Expr): int =
  case expression.kind
  of numberExpr:
    expression.value
  of addExpr:
    interpret(expression.left) + interpret(expression.right)

let expression = add(add(number(2), number(3)), number(4))
let result = interpret(expression)
if result != 9:
  raise newException(ValueError, "Interpreter expected 9, got " & $result)
echo "interpreter=9"
