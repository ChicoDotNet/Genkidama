class LegacyFahrenheitSensor:
    def read_fahrenheit(self) -> int:
        return 86


class FahrenheitSensorAdapter:
    def __init__(self, sensor: LegacyFahrenheitSensor) -> None:
        self._sensor = sensor

    def read_celsius(self) -> int:
        return (self._sensor.read_fahrenheit() - 32) * 5 // 9


def main() -> None:
    legacy = LegacyFahrenheitSensor()
    reader = FahrenheitSensorAdapter(legacy)
    print(f"legacy={legacy.read_fahrenheit()}F")
    print(f"adapted={reader.read_celsius()}C")


if __name__ == "__main__":
    main()
