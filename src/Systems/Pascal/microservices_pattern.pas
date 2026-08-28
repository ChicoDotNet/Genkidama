unit Microservices_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
function Reserve(var Stock:Integer; Quantity:Integer):Boolean;begin Result:=Quantity<=Stock;if Result then Dec(Stock,Quantity);end;
function Place(var Stock:Integer; Quantity:Integer):String;begin if Reserve(Stock,Quantity)then Result:='confirmed' else Result:='rejected';end;
function Run:Boolean;var Stock:Integer;Status:String;begin Stock:=7;Status:=Place(Stock,2);Result:=(Status='confirmed')and(Stock=5);end;
end.
