class LegacyFahrenheitSensor:
    def read_fahrenheit(self):
        return 86


class FahrenheitSensorAdapter:
    def __init__(self, adaptee):
        self._adaptee = adaptee

    def read_celsius(self):
        fahrenheit = self._adaptee.read_fahrenheit()
        return ((fahrenheit - 32) * 5) // 9


legacy = LegacyFahrenheitSensor()
reader = FahrenheitSensorAdapter(legacy)

print("legacy={}F".format(legacy.read_fahrenheit()))
print("adapted={}C".format(reader.read_celsius()))
