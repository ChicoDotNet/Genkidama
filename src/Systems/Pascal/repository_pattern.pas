unit Repository_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
function Run:Boolean;var Ids:array[1..2]of Integer;Names:array[1..2]of String;Found:String;I:Integer;begin Ids[1]:=1;Ids[2]:=2;Names[1]:='Ada';Names[2]:='Grace';Found:='';for I:=1 to 2 do if Ids[I]=2 then Found:=Names[I];Result:=Found='Grace';end;
end.
