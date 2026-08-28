unit Object_Pool_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TPool=record Values:array[1..2]of Integer;Size:Integer;end;
function Borrow(var Pool:TPool):Integer;begin Result:=Pool.Values[Pool.Size];Dec(Pool.Size);end;
procedure Release(var Pool:TPool;Item:Integer);begin Inc(Pool.Size);Pool.Values[Pool.Size]:=Item;end;
function Run:Boolean;var Pool:TPool;Item:Integer;begin Pool.Values[1]:=1;Pool.Values[2]:=2;Pool.Size:=2;Item:=Borrow(Pool);Release(Pool,Item);Result:=(Item=2)and(Pool.Size=2)and(Pool.Values[2]=2);end;
end.
