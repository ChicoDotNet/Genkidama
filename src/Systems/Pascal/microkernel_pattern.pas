unit Microkernel_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TPlugin=function(Value:Integer):Integer;
function DoubleValue(Value:Integer):Integer;begin Result:=Value*2;end;
function SquareValue(Value:Integer):Integer;begin Result:=Value*Value;end;
function Run:Boolean;var Plugins:array[1..2]of TPlugin;begin Plugins[1]:=@DoubleValue;Plugins[2]:=@SquareValue;Result:=(Plugins[1](4)=8)and(Plugins[2](4)=16);end;
end.
