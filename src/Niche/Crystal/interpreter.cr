abstract class Expr
  abstract def interpret : Int32
end

class NumberExpr < Expr
  def initialize(@value : Int32)
  end

  def interpret : Int32
    @value
  end
end

class AddExpr < Expr
  def initialize(@left : Expr, @right : Expr)
  end

  def interpret : Int32
    @left.interpret + @right.interpret
  end
end

expression = AddExpr.new(AddExpr.new(NumberExpr.new(2), NumberExpr.new(3)), NumberExpr.new(4))
result = expression.interpret
raise "Interpreter expected 9, got #{result}" unless result == 9
puts "interpreter=9"
