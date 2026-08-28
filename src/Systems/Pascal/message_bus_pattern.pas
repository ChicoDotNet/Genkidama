unit Message_Bus_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
procedure Publish(const Topic:String;Id:Integer;out Audit,Billing:String);begin Audit:='audit:'+Topic+':'+IntToStr(Id);Billing:='billing:'+Topic+':'+IntToStr(Id);end;
function Run:Boolean;var A,B:String;begin Publish('order-created',42,A,B);Result:=A+'>'+B='audit:order-created:42>billing:order-created:42';end;
end.
