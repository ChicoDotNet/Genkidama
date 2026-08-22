interface TemperatureReader {
  readCelsius(): number;
}

class LegacyFahrenheitSensor {
  readFahrenheit(): number {
    return 86;
  }
}

class FahrenheitSensorAdapter implements TemperatureReader {
  constructor(private readonly sensor: LegacyFahrenheitSensor) {}

  readCelsius(): number {
    return Math.trunc(((this.sensor.readFahrenheit() - 32) * 5) / 9);
  }
}

const legacy = new LegacyFahrenheitSensor();
const reader: TemperatureReader = new FahrenheitSensorAdapter(legacy);
console.log(`legacy=${legacy.readFahrenheit()}F`);
console.log(`adapted=${reader.readCelsius()}C`);
