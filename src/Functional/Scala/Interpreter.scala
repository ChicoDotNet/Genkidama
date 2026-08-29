sealed trait Expr {
  def interpret: Int
}

final case class Number(value: Int) extends Expr {
  override def interpret: Int = value
}

final case class Add(left: Expr, right: Expr) extends Expr {
  override def interpret: Int = left.interpret + right.interpret
}

@main def interpreterExample(): Unit = {
  val expr: Expr = Add(Number(2), Add(Number(3), Number(4)))
  val value = expr.interpret
  require(value == 9, s"expected 9, got $value")
  println(s"value=$value")
}
