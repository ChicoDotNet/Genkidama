type
  LegacyFahrenheitSensor = object
  FahrenheitSensorAdapter = object
    adaptee: LegacyFahrenheitSensor

proc readFahrenheit(_: LegacyFahrenheitSensor): int =
  86

proc readCelsius(adapter: FahrenheitSensorAdapter): int =
  let fahrenheit = adapter.adaptee.readFahrenheit()
  ((fahrenheit - 32) * 5) div 9

let legacy = LegacyFahrenheitSensor()
let adapter = FahrenheitSensorAdapter(adaptee: legacy)

echo "legacy=", legacy.readFahrenheit(), "F"
echo "adapted=", adapter.readCelsius(), "C"
