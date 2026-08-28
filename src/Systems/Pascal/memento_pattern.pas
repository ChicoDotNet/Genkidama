unit Memento_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
function Run:Boolean;
var State,Snapshot:String; Published:Boolean;
begin State:='draft';Snapshot:=State;State:='published';Published:=State='published';State:=Snapshot;Result:=Published and(State='draft');end;
end.
