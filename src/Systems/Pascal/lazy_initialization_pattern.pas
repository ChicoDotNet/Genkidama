unit Lazy_Initialization_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TLazyValue=record Ready:Boolean;Builds,Value:Integer;end;
procedure Ensure(var State:TLazyValue);begin if not State.Ready then begin State.Ready:=True;Inc(State.Builds);State.Value:=42;end;end;
function Run:Boolean;var State:TLazyValue;begin State.Ready:=False;State.Builds:=0;State.Value:=0;Ensure(State);Ensure(State);Result:=State.Ready and(State.Builds=1)and(State.Value=42);end;
end.
