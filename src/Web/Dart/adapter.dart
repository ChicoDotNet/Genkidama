abstract interface class TemperatureReader {
  int readCelsius();
}

final class LegacyFahrenheitSensor {
  int readFahrenheit() => 86;
}

final class FahrenheitSensorAdapter implements TemperatureReader {
  FahrenheitSensorAdapter(this._adaptee);

  final LegacyFahrenheitSensor _adaptee;

  @override
  int readCelsius() => ((_adaptee.readFahrenheit() - 32) * 5) ~/ 9;
}

void main() {
  final legacy = LegacyFahrenheitSensor();
  final TemperatureReader reader = FahrenheitSensorAdapter(legacy);

  print('legacy=${legacy.readFahrenheit()}F');
  print('adapted=${reader.readCelsius()}C');
}
