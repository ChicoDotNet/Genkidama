#include <iostream>

class TemperatureReader {
public:
    virtual ~TemperatureReader() = default;
    virtual int readCelsius() const = 0;
};

class LegacyFahrenheitSensor {
public:
    int readFahrenheit() const { return 86; }
};

class FahrenheitSensorAdapter final : public TemperatureReader {
public:
    explicit FahrenheitSensorAdapter(const LegacyFahrenheitSensor& sensor) : sensor_(sensor) {}

    int readCelsius() const override {
        return (sensor_.readFahrenheit() - 32) * 5 / 9;
    }

private:
    const LegacyFahrenheitSensor& sensor_;
};

int main() {
    const LegacyFahrenheitSensor legacy;
    const FahrenheitSensorAdapter reader{legacy};
    std::cout << "legacy=" << legacy.readFahrenheit() << "F\n";
    std::cout << "adapted=" << reader.readCelsius() << "C\n";
}
