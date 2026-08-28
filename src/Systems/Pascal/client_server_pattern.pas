unit Client_Server_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TResponse=record Status,Stock:Integer;end;
function Server(const Key:String):TResponse;begin if Key='sku-1' then begin Result.Status:=200;Result.Stock:=7;end else begin Result.Status:=404;Result.Stock:=0;end;end;
function Run:Boolean;var R:TResponse;begin R:=Server('sku-1');Result:=(R.Status=200)and(R.Stock=7);end;
end.
