unit MVP_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TModel=record Count:Integer;end;TView=record TextCount:Integer;end;
procedure Present(var Model:TModel;var View:TView);begin Inc(Model.Count);View.TextCount:=Model.Count;end;
function Run:Boolean;var M:TModel;V:TView;begin M.Count:=0;V.TextCount:=0;Present(M,V);Result:=(M.Count=1)and(V.TextCount=1);end;
end.
