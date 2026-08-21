#include <stdio.h>

typedef struct {
    int (*read_fahrenheit)(void);
} LegacyFahrenheitSensor;

typedef struct {
    const LegacyFahrenheitSensor *legacy;
    int (*read_celsius)(const LegacyFahrenheitSensor *legacy);
} TemperatureReader;

static int legacy_read_fahrenheit(void) {
    return 86;
}

static int adapter_read_celsius(const LegacyFahrenheitSensor *legacy) {
    const int fahrenheit = legacy->read_fahrenheit();
    return (fahrenheit - 32) * 5 / 9;
}

int main(void) {
    const LegacyFahrenheitSensor legacy = {legacy_read_fahrenheit};
    const TemperatureReader reader = {&legacy, adapter_read_celsius};

    printf("legacy=%dF\n", legacy.read_fahrenheit());
    printf("adapted=%dC\n", reader.read_celsius(reader.legacy));
    return 0;
}
