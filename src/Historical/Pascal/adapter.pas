program Adapter;

{$mode objfpc}{$H+}

type
  TLegacyFahrenheitSensor = class
  public
    function ReadFahrenheit: Integer;
  end;

  TTemperatureReader = class abstract
  public
    function ReadCelsius: Integer; virtual; abstract;
  end;

  TFahrenheitSensorAdapter = class(TTemperatureReader)
  private
    FLegacy: TLegacyFahrenheitSensor;
  public
    constructor Create(ALegacy: TLegacyFahrenheitSensor);
    function ReadCelsius: Integer; override;
  end;

function TLegacyFahrenheitSensor.ReadFahrenheit: Integer;
begin
  Result := 86;
end;

constructor TFahrenheitSensorAdapter.Create(ALegacy: TLegacyFahrenheitSensor);
begin
  inherited Create;
  FLegacy := ALegacy;
end;

function TFahrenheitSensorAdapter.ReadCelsius: Integer;
begin
  Result := Round((FLegacy.ReadFahrenheit - 32) * 5.0 / 9.0);
end;

var
  Legacy: TLegacyFahrenheitSensor;
  Reader: TTemperatureReader;
begin
  Legacy := TLegacyFahrenheitSensor.Create;
  Reader := TFahrenheitSensorAdapter.Create(Legacy);
  try
    WriteLn('legacy=', Legacy.ReadFahrenheit, 'F');
    WriteLn('adapted=', Reader.ReadCelsius, 'C');
  finally
    Reader.Free;
    Legacy.Free;
  end;
end.
