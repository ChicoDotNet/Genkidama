class LegacyFahrenheitSensor {
  readFahrenheit() {
    return 86;
  }
}

class FahrenheitSensorAdapter {
  constructor(sensor) {
    this.sensor = sensor;
  }

  readCelsius() {
    return Math.trunc(((this.sensor.readFahrenheit() - 32) * 5) / 9);
  }
}

const legacy = new LegacyFahrenheitSensor();
const reader = new FahrenheitSensorAdapter(legacy);
console.log(`legacy=${legacy.readFahrenheit()}F`);
console.log(`adapted=${reader.readCelsius()}C`);
