program SingletonExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

type
  TRegistry = class
  strict private
    class var FInstance: TRegistry;
    FCount: Integer;
    constructor CreatePrivate;
  public
    class function Instance: TRegistry; static;
    procedure Increment;
    property Count: Integer read FCount;
  end;

constructor TRegistry.CreatePrivate;
begin
  inherited Create;
  FCount := 0;
end;

class function TRegistry.Instance: TRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TRegistry.CreatePrivate;
  Result := FInstance;
end;

procedure TRegistry.Increment;
begin
  Inc(FCount);
end;

var
  FirstRegistry: TRegistry;
  SecondRegistry: TRegistry;
begin
  FirstRegistry := TRegistry.Instance;
  SecondRegistry := TRegistry.Instance;
  FirstRegistry.Increment;

  if FirstRegistry = SecondRegistry then
    Writeln('same=true')
  else
    Writeln('same=false');
  Writeln('count=', SecondRegistry.Count);
end.
