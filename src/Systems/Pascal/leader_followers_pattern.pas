unit Leader_Followers_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
function Run:Boolean;var Workers:array[1..3]of String;Events:array[1..3]of Char;Handled:String;I:Integer;begin Workers[1]:='worker-1';Workers[2]:='worker-2';Workers[3]:='worker-3';Events[1]:='a';Events[2]:='b';Events[3]:='c';Handled:='';for I:=1 to 3 do begin if Handled<>'' then Handled:=Handled+'>';Handled:=Handled+Workers[I]+':'+Events[I];end;Result:=(Handled='worker-1:a>worker-2:b>worker-3:c')and(Workers[1]='worker-1');end;
end.
