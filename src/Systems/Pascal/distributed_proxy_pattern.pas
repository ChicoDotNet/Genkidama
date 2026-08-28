unit Distributed_Proxy_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
function Remote(const SKU:String):Integer;begin if SKU='sku-1' then Result:=7 else Result:=0;end;
function Proxy(const SKU:String):Integer;begin Result:=Remote(SKU);end;
function Run:Boolean;begin Result:=(Proxy('sku-1')=7)and(Proxy('missing')=0);end;
end.
