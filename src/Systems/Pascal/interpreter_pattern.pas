unit Interpreter_Pattern;
{$mode objfpc}{$H+}
interface
function Run: Boolean;
implementation
type TExprKind=(ekLiteral,ekAdd,ekMultiply);
     TExpr=record Kind:TExprKind; Left,Right,Value:Integer; end;
function Eval(const Expr:TExpr):Integer;
begin case Expr.Kind of ekLiteral:Result:=Expr.Value; ekAdd:Result:=Expr.Left+Expr.Right; ekMultiply:Result:=Expr.Left*Expr.Right; end; end;
function Run:Boolean;
var ProductExpr,SumExpr:TExpr; Product:Integer;
begin
  ProductExpr.Kind:=ekMultiply; ProductExpr.Left:=3; ProductExpr.Right:=4; Product:=Eval(ProductExpr);
  SumExpr.Kind:=ekAdd; SumExpr.Left:=7; SumExpr.Right:=Product;
  Result:=Eval(SumExpr)=19;
end;
end.
