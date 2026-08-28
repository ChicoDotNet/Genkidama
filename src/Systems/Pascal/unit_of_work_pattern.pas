unit Unit_Of_Work_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
function Run:Boolean;var Pending,Store:array[1..2]of Integer;begin Pending[1]:=2;Pending[2]:=3;Store:=Pending;Pending[1]:=0;Pending[2]:=0;Result:=(Store[1]=2)and(Store[2]=3)and(Pending[1]=0)and(Pending[2]=0);end;
end.
