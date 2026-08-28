unit Publish_Subscribe_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
procedure Publish(Id:Integer;out Warehouse,Analytics:Integer);begin Warehouse:=100+Id;Analytics:=200+Id;end;
function Run:Boolean;var W,A:Integer;begin Publish(51,W,A);Result:=(W=151)and(A=251);end;
end.
