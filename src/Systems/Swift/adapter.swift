protocol TemperatureReader {
    func readCelsius() -> Int
}

final class LegacyFahrenheitSensor {
    func readFahrenheit() -> Int { 86 }
}

final class FahrenheitSensorAdapter: TemperatureReader {
    private let sensor: LegacyFahrenheitSensor

    init(sensor: LegacyFahrenheitSensor) {
        self.sensor = sensor
    }

    func readCelsius() -> Int {
        (sensor.readFahrenheit() - 32) * 5 / 9
    }
}

let legacy = LegacyFahrenheitSensor()
let reader: TemperatureReader = FahrenheitSensorAdapter(sensor: legacy)
print("legacy=\(legacy.readFahrenheit())F")
print("adapted=\(reader.readCelsius())C")
