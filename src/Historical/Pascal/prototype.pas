program PrototypeExample;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TStringArray = array of string;

  TServiceProfile = record
    Name: string;
    Features: TStringArray;
  end;

function CloneProfile(const Source: TServiceProfile): TServiceProfile;
var
  I: Integer;
begin
  Result.Name := Source.Name;
  SetLength(Result.Features, Length(Source.Features));
  for I := 0 to High(Source.Features) do
    Result.Features[I] := Source.Features[I];
end;

function Describe(const Profile: TServiceProfile): string;
var
  I: Integer;
begin
  Result := Profile.Name + ': ';
  for I := 0 to High(Profile.Features) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + Profile.Features[I];
  end;
end;

var
  OriginalProfile, Canary: TServiceProfile;
begin
  OriginalProfile.Name := 'orders';
  SetLength(OriginalProfile.Features, 1);
  OriginalProfile.Features[0] := 'metrics';

  Canary := CloneProfile(OriginalProfile);
  Canary.Name := 'orders-canary';
  SetLength(Canary.Features, 2);
  Canary.Features[1] := 'tracing';

  WriteLn('original=' + Describe(OriginalProfile));
  WriteLn('clone=' + Describe(Canary));
end.
