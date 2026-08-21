extends SceneTree

class LegacyFahrenheitSensor:
    func read_fahrenheit() -> int:
        return 86

class FahrenheitSensorAdapter:
    var adaptee: LegacyFahrenheitSensor

    func _init(sensor: LegacyFahrenheitSensor) -> void:
        adaptee = sensor

    func read_celsius() -> int:
        return int((adaptee.read_fahrenheit() - 32) * 5 / 9)

func _init() -> void:
    var legacy := LegacyFahrenheitSensor.new()
    var reader := FahrenheitSensorAdapter.new(legacy)
    print("legacy=%dF" % legacy.read_fahrenheit())
    print("adapted=%dC" % reader.read_celsius())
    quit()
