unit PAC_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TAgent=record Abstraction,Control:Integer;end;
function Present(const Agent:TAgent):Integer;begin Result:=Agent.Abstraction+Agent.Control;end;
function Run:Boolean;var Child,Root:TAgent;begin Child.Abstraction:=40;Child.Control:=2;Root.Abstraction:=Present(Child);Root.Control:=0;Result:=(Present(Child)=42)and(Present(Root)=42);end;
end.
