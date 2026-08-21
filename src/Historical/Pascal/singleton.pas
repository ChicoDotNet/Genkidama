program SingletonExample;

{$mode objfpc}{$H+}

type
  PRegistry = ^TRegistry;
  TRegistry = record
    Count: Integer;
  end;

var
  SharedRegistry: TRegistry;

function Instance: PRegistry;
begin
  Result := @SharedRegistry;
end;

procedure Increment(Registry: PRegistry);
begin
  Inc(Registry^.Count);
end;

var
  FirstRegistry: PRegistry;
  SecondRegistry: PRegistry;
begin
  FirstRegistry := Instance;
  SecondRegistry := Instance;
  Increment(FirstRegistry);

  if FirstRegistry = SecondRegistry then
    WriteLn('same=true')
  else
    WriteLn('same=false');
  WriteLn('count=', SecondRegistry^.Count);
end.
