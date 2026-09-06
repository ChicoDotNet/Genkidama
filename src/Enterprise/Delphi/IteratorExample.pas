program IteratorExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TNumberIterator = class
  private
    FValues: array[0..2] of Integer;
    FIndex: Integer;
  public
    constructor Create;
    function HasNext: Boolean;
    function Next: Integer;
  end;

constructor TNumberIterator.Create;
begin
  inherited Create;
  FValues[0] := 10;
  FValues[1] := 20;
  FValues[2] := 30;
  FIndex := 0;
end;

function TNumberIterator.HasNext: Boolean;
begin
  Result := FIndex <= High(FValues);
end;

function TNumberIterator.Next: Integer;
begin
  if not HasNext then
    raise Exception.Create('Iterator exhausted');
  Result := FValues[FIndex];
  Inc(FIndex);
end;

var
  It: TNumberIterator;
  A, B, C: Integer;
begin
  It := TNumberIterator.Create;
  try
    A := It.Next;
    B := It.Next;
    C := It.Next;
    if (A <> 10) or (B <> 20) or (C <> 30) or It.HasNext then
      raise Exception.Create('Iterator contract failed');
    Writeln('iterator=10,20,30');
  finally
    It.Free;
  end;
end.
