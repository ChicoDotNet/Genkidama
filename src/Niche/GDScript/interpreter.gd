extends SceneTree

class Expr:
	extends RefCounted
	func interpret() -> int:
		return 0

class NumberExpr:
	extends Expr
	var value: int

	func _init(number: int) -> void:
		value = number

	func interpret() -> int:
		return value

class AddExpr:
	extends Expr
	var left: Expr
	var right: Expr

	func _init(lhs: Expr, rhs: Expr) -> void:
		left = lhs
		right = rhs

	func interpret() -> int:
		return left.interpret() + right.interpret()

func _init() -> void:
	var expression: Expr = AddExpr.new(
		AddExpr.new(NumberExpr.new(2), NumberExpr.new(3)),
		NumberExpr.new(4)
	)
	var value := expression.interpret()
	assert(value == 9)
	print("interpreter=%d" % value)
	quit()
