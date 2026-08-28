unit Dependency_Injection_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TClock=function:String;
function FixedClock:String;begin Result:='10:00';end;
function Service(Clock:TClock):String;begin Result:='at:'+Clock();end;
function Run:Boolean;begin Result:=Service(@FixedClock)='at:10:00';end;
end.
