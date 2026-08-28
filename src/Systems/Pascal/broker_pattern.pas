unit Broker_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
type TService=(svcInventory,svcCustomer);
function Route(Service:TService;Key:Integer):String;begin if Service=svcInventory then Result:='inventory:sku-'+IntToStr(Key)+'=7' else Result:='customer:'+IntToStr(Key)+'=active';end;
function Run:Boolean;begin Result:=(Route(svcInventory,1)='inventory:sku-1=7')and(Route(svcCustomer,17)='customer:17=active');end;
end.
