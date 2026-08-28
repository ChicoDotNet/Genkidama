unit Half_Sync_Half_Async_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
function SyncHandle(Job:Integer):String;begin Result:='done:job-'+IntToStr(Job);end;
function Run:Boolean;var Queue:array[1..3]of Integer;Output:String;I:Integer;begin Queue[1]:=1;Queue[2]:=2;Queue[3]:=3;Output:='';for I:=1 to 3 do begin if Output<>'' then Output:=Output+'>';Output:=Output+SyncHandle(Queue[I]);end;Result:=Output='done:job-1>done:job-2>done:job-3';end;
end.
