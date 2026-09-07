program StatePattern;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TGateState = (gsLocked, gsUnlocked, gsInvalid);

function Transition(CurrentState: TGateState; const Action: string): TGateState;
begin
  case CurrentState of
    gsLocked:
      if Action = 'coin' then
        Result := gsUnlocked
      else
        Result := gsLocked;
    gsUnlocked:
      if Action = 'push' then
        Result := gsLocked
      else
        Result := gsUnlocked;
  else
    Result := gsInvalid;
  end;
end;

procedure RequireState(Actual, Expected: TGateState; const MessageText: string);
begin
  if Actual <> Expected then
    raise Exception.Create(MessageText);
end;

var
  State: TGateState;
begin
  State := gsLocked;
  RequireState(State, gsLocked, 'initial state must be locked');

  State := Transition(State, 'push');
  RequireState(State, gsLocked, 'push while locked must preserve state');

  State := Transition(State, 'coin');
  RequireState(State, gsUnlocked, 'coin while locked must unlock');

  State := Transition(State, 'coin');
  RequireState(State, gsUnlocked, 'duplicate coin must preserve unlocked state');

  State := Transition(State, 'push');
  RequireState(State, gsLocked, 'push while unlocked must lock');

  State := Transition(gsInvalid, 'coin');
  RequireState(State, gsInvalid, 'unknown state must remain invalid');

  Writeln('delphi-state: passed');
end.
