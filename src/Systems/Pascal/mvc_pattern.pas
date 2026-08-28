unit MVC_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
type TModel=record Count:Integer;end;
function Render(const Model:TModel):String;begin Result:='count='+IntToStr(Model.Count);end;
procedure Increment(var Model:TModel);begin Inc(Model.Count);end;
function Run:Boolean;var Model:TModel;Before:String;begin Model.Count:=0;Before:=Render(Model);Increment(Model);Result:=(Before='count=0')and(Render(Model)='count=1');end;
end.
