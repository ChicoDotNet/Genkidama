unit State_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TGate=(gsLocked,gsUnlocked); TAction=(gaLock,gaUnlock);
function Transition(State:TGate; Action:TAction):TGate;
begin Result:=State;if(State=gsLocked)and(Action=gaUnlock)then Result:=gsUnlocked else if(State=gsUnlocked)and(Action=gaLock)then Result:=gsLocked;end;
function Run:Boolean;var State:TGate;begin State:=Transition(gsLocked,gaUnlock);State:=Transition(State,gaLock);Result:=State=gsLocked;end;
end.
