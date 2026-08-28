unit Command_Pattern;
{$mode objfpc}{$H+}
interface
function Run: Boolean;
implementation
type TCommandKind = (ckDeposit, ckWithdraw);
     TCommand = record Kind: TCommandKind; Amount: Integer; end;
function Execute(Balance: Integer; const Cmd: TCommand): Integer;
begin if Cmd.Kind=ckDeposit then Result:=Balance+Cmd.Amount else Result:=Balance-Cmd.Amount; end;
function Run: Boolean;
var Queue: array[1..2] of TCommand; Balance: Integer;
begin
  Queue[1].Kind:=ckDeposit; Queue[1].Amount:=50;
  Queue[2].Kind:=ckWithdraw; Queue[2].Amount:=20;
  Balance:=100; Balance:=Execute(Balance,Queue[1]); Balance:=Execute(Balance,Queue[2]);
  Result:=(Balance=130) and (Execute(150,Queue[2])=130);
end;
end.
