unit Strategy_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TStrategy=function(Value:Integer):Integer;
function Regular(Value:Integer):Integer;begin Result:=Value;end;
function Discount(Value:Integer):Integer;begin Result:=Value*80 div 100;end;
function Price(Value:Integer; Strategy:TStrategy):Integer;begin Result:=Strategy(Value);end;
function Run:Boolean;begin Result:=(Price(100,@Regular)=100)and(Price(100,@Discount)=80);end;
end.
