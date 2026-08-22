interface TemperatureReader {
    int readCelsius()
}

class LegacyFahrenheitSensor {
    int readFahrenheit() {
        86
    }
}

class FahrenheitSensorAdapter implements TemperatureReader {
    private final LegacyFahrenheitSensor adaptee

    FahrenheitSensorAdapter(LegacyFahrenheitSensor adaptee) {
        this.adaptee = adaptee
    }

    @Override
    int readCelsius() {
        ((adaptee.readFahrenheit() - 32) * 5 / 9) as int
    }
}

def legacy = new LegacyFahrenheitSensor()
def reader = new FahrenheitSensorAdapter(legacy)

println "legacy=${legacy.readFahrenheit()}F"
println "adapted=${reader.readCelsius()}C"
