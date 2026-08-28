unit Enterprise_Facade_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
function CRM(Id:Integer):String;begin Result:='crm:create:'+IntToStr(Id);end;
function Billing(Id:Integer):String;begin Result:='billing:open:'+IntToStr(Id);end;
function Onboard(Id:Integer):String;begin Result:=CRM(Id)+'>'+Billing(Id);end;
function Run:Boolean;begin Result:=Onboard(77)='crm:create:77>billing:open:77';end;
end.
