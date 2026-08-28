unit Observer_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
function Audit(Id:Integer):String;begin Result:='audit:'+IntToStr(Id);end;
function Dashboard(Id:Integer):String;begin Result:='dashboard:'+IntToStr(Id);end;
function Run:Boolean;begin Result:=Audit(42)+'>'+Dashboard(42)='audit:42>dashboard:42';end;
end.
