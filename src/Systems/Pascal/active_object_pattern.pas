unit Active_Object_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TOperation=(opAddThree,opTimesFour);
function Execute(Value:Integer;Operation:TOperation):Integer;begin if Operation=opAddThree then Result:=Value+3 else Result:=Value*4;end;
function Run:Boolean;var Queue:array[1..2]of TOperation;Value,I:Integer;begin Queue[1]:=opAddThree;Queue[2]:=opTimesFour;Value:=0;for I:=1 to 2 do Value:=Execute(Value,Queue[I]);Result:=Value=12;end;
end.
