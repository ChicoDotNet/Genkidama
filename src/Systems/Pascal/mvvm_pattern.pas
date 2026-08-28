unit MVVM_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
type TModel=record Amount:Integer;end;
function BindAmount(const Model:TModel):String;begin Result:='$'+IntToStr(Model.Amount)+'.00';end;
function Run:Boolean;var Model:TModel;Before:String;begin Model.Amount:=10;Before:=BindAmount(Model);Inc(Model.Amount,5);Result:=(Before='$10.00')and(BindAmount(Model)='$15.00');end;
end.
