sealed class Expr {
  int interpret();
}

final class NumberExpr implements Expr {
  const NumberExpr(this.value);

  final int value;

  @override
  int interpret() => value;
}

final class AddExpr implements Expr {
  const AddExpr(this.left, this.right);

  final Expr left;
  final Expr right;

  @override
  int interpret() => left.interpret() + right.interpret();
}

void main() {
  const Expr expression = AddExpr(AddExpr(NumberExpr(2), NumberExpr(3)), NumberExpr(4));
  final result = expression.interpret();
  if (result != 9) {
    throw StateError('Interpreter expected 9, got $result');
  }
  print('interpreter=9');
}
