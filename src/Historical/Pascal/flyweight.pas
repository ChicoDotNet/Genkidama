program FlyweightExample;
{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TStyle = record
    Font: string;
    Size: Integer;
    Color: string;
  end;

var
  Styles: array[0..1] of TStyle;
  Keys: array[0..1] of string;
  Count: Integer = 0;

function StyleKey(const Font: string; Size: Integer; const Color: string): string;
begin
  Result := Font + '|' + IntToStr(Size) + '|' + Color;
end;

function GetStyle(const Font: string; Size: Integer; const Color: string): Integer;
var
  Key: string;
  I: Integer;
begin
  Key := StyleKey(Font, Size, Color);
  for I := 0 to Count - 1 do
    if Keys[I] = Key then Exit(I);
  Keys[Count] := Key;
  Styles[Count].Font := Font;
  Styles[Count].Size := Size;
  Styles[Count].Color := Color;
  Result := Count;
  Inc(Count);
end;

var
  Red1, Red2, Blue: Integer;
begin
  Red1 := GetStyle('Inter', 12, 'red');
  Red2 := GetStyle('Inter', 12, 'red');
  Blue := GetStyle('Inter', 12, 'blue');
  if Blue < 0 then Halt(1);
  WriteLn('styles=', Count, ';shared=', LowerCase(BoolToStr(Red1 = Red2, True)), ';text=ABC');
end.
