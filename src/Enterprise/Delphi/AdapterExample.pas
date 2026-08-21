program AdapterExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

type
  ITemperatureReader = interface
    ['{677B9650-6C1D-4F5D-AD43-CEDAA0877C2B}']
    function ReadCelsius: Integer;
  end;

  TLegacyFahrenheitSensor = class
  public
    function ReadFahrenheit: Integer;
  end;

  TFahrenheitSensorAdapter = class(TInterfacedObject, ITemperatureReader)
  private
    FAdaptee: TLegacyFahrenheitSensor;
  public
    constructor Create(AAdaptee: TLegacyFahrenheitSensor);
    function ReadCelsius: Integer;
  end;

function TLegacyFahrenheitSensor.ReadFahrenheit: Integer;
begin
  Result := 86;
end;

constructor TFahrenheitSensorAdapter.Create(AAdaptee: TLegacyFahrenheitSensor);
begin
  inherited Create;
  FAdaptee := AAdaptee;
end;

function TFahrenheitSensorAdapter.ReadCelsius: Integer;
var
  Fahrenheit: Integer;
begin
  Fahrenheit := FAdaptee.ReadFahrenheit;
  Result := ((Fahrenheit - 32) * 5) div 9;
end;

var
  Legacy: TLegacyFahrenheitSensor;
  Reader: ITemperatureReader;
begin
  Legacy := TLegacyFahrenheitSensor.Create;
  try
    Reader := TFahrenheitSensorAdapter.Create(Legacy);
    Writeln('legacy=', Legacy.ReadFahrenheit, 'F');
    Writeln('adapted=', Reader.ReadCelsius, 'C');
    Reader := nil;
  finally
    Legacy.Free;
  end;
end.
