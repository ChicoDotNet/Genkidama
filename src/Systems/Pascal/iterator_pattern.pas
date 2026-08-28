unit Iterator_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TIntArray=array[1..3] of Integer;
     TIterator=record Values:TIntArray; Cursor:Integer; end;
function Next(var It:TIterator; out Value:Integer):Boolean;
begin
  Result:=It.Cursor<3;
  if Result then begin Inc(It.Cursor); Value:=It.Values[It.Cursor]; end;
end;
function Run:Boolean;
var It:TIterator; Seen:TIntArray; I,V:Integer;
begin
  It.Values[1]:=10;It.Values[2]:=20;It.Values[3]:=30;It.Cursor:=0;I:=0;
  while Next(It,V) do begin Inc(I);Seen[I]:=V;end;
  Result:=(I=3)and(Seen[1]=10)and(Seen[2]=20)and(Seen[3]=30);
end;
end.
