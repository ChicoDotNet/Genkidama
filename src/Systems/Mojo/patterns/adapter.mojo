@fieldwise_init
struct LegacyFahrenheitSensor(Copyable):
    var reading: Int

    def fahrenheit(self) -> Int:
        return self.reading


@fieldwise_init
struct CelsiusAdapter(Copyable):
    var adaptee: LegacyFahrenheitSensor

    def celsius(self) -> Int:
        return (self.adaptee.fahrenheit() - 32) * 5 // 9
