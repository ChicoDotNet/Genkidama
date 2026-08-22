interface TemperatureReader {
    int readCelsius();
}

final class LegacyFahrenheitSensor {
    int readFahrenheit() {
        return 86;
    }
}

final class FahrenheitSensorAdapter implements TemperatureReader {
    private final LegacyFahrenheitSensor sensor;

    FahrenheitSensorAdapter(LegacyFahrenheitSensor sensor) {
        this.sensor = sensor;
    }

    @Override
    public int readCelsius() {
        return (sensor.readFahrenheit() - 32) * 5 / 9;
    }
}

public final class AdapterExample {
    public static void main(String[] args) {
        var legacy = new LegacyFahrenheitSensor();
        TemperatureReader reader = new FahrenheitSensorAdapter(legacy);
        System.out.printf("legacy=%dF%n", legacy.readFahrenheit());
        System.out.printf("adapted=%dC%n", reader.readCelsius());
    }
}
