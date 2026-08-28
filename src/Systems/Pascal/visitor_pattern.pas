unit Visitor_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses Math;
type TShapeKind=(skCircle,skRectangle); TShape=record Kind:TShapeKind; A,B:Double; end;
function Area(const Shape:TShape):Double;begin if Shape.Kind=skCircle then Result:=Pi*Shape.A*Shape.A else Result:=Shape.A*Shape.B;end;
function Run:Boolean;var C,R:TShape; Total:Double;begin C.Kind:=skCircle;C.A:=2;C.B:=0;R.Kind:=skRectangle;R.A:=3;R.B:=4;Total:=Area(C)+Area(R);Result:=Abs(Total-(4*Pi+12))<1e-9;end;
end.
