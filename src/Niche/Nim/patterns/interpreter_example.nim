type
  ExprKind = enum ekLit, ekAdd, ekMul
  Expr = ref object
    kind: ExprKind
    value: int
    left, right: Expr
proc eval(e: Expr): int =
  case e.kind
  of ekLit: e.value
  of ekAdd: eval(e.left) + eval(e.right)
  of ekMul: eval(e.left) * eval(e.right)
proc run*(): bool =
  let e = Expr(kind: ekAdd, left: Expr(kind: ekLit, value: 7), right: Expr(kind: ekMul, left: Expr(kind: ekLit, value: 3), right: Expr(kind: ekLit, value: 4)))
  eval(e) == 19
