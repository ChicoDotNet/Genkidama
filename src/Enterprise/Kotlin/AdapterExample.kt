interface TemperatureReader {
    fun readCelsius(): Int
}

class LegacyFahrenheitSensor {
    fun readFahrenheit(): Int = 86
}

class FahrenheitSensorAdapter(
    private val sensor: LegacyFahrenheitSensor,
) : TemperatureReader {
    override fun readCelsius(): Int = (sensor.readFahrenheit() - 32) * 5 / 9
}

fun main() {
    val legacy = LegacyFahrenheitSensor()
    val reader: TemperatureReader = FahrenheitSensorAdapter(legacy)
    println("legacy=${legacy.readFahrenheit()}F")
    println("adapted=${reader.readCelsius()}C")
}
