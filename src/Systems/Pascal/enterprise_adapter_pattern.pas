unit Enterprise_Adapter_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TLegacy=record Code,Cents:Integer;end; TCanonical=record Id:Integer;Amount:Double;end;
function Adapt(const Legacy:TLegacy):TCanonical;begin Result.Id:=Legacy.Code;Result.Amount:=Legacy.Cents/100.0;end;
function Run:Boolean;var L:TLegacy;C:TCanonical;begin L.Code:=17;L.Cents:=1250;C:=Adapt(L);Result:=(C.Id=17)and(Abs(C.Amount-12.5)<1e-9);end;
end.
