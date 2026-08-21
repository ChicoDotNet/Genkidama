program Bridge;
{$mode objfpc}{$H+}

uses SysUtils;

type
  TDeviceAction = function: string of object;

  TDevice = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    function TurnOn: string;
    function Mute: string;
  end;

constructor TDevice.Create(const AName: string);
begin
  FName := AName;
end;

function TDevice.TurnOn: string;
begin
  Result := FName + ':on';
end;

function TDevice.Mute: string;
begin
  Result := FName + ':muted';
end;

function ExecuteRemote(Action: TDeviceAction): string;
begin
  Result := Action();
end;

var
  Tv, Radio: TDevice;
begin
  Tv := TDevice.Create('TV');
  Radio := TDevice.Create('Radio');
  try
    Writeln('basic-tv=', ExecuteRemote(@Tv.TurnOn));
    Writeln('basic-radio=', ExecuteRemote(@Radio.TurnOn));
    Writeln('mute-tv=', ExecuteRemote(@Tv.Mute));
    Writeln('mute-radio=', ExecuteRemote(@Radio.Mute));
  finally
    Tv.Free;
    Radio.Free;
  end;
end.
