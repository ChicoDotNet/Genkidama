trait TemperatureReader {
  def readCelsius(): Int
}

final class LegacyFahrenheitSensor {
  def readFahrenheit(): Int = 86
}

final class FahrenheitSensorAdapter(legacy: LegacyFahrenheitSensor) extends TemperatureReader {
  override def readCelsius(): Int =
    Math.round((legacy.readFahrenheit() - 32) * 5.0 / 9.0).toInt
}

object Adapter {
  def main(args: Array[String]): Unit = {
    val legacy = new LegacyFahrenheitSensor
    val reader: TemperatureReader = new FahrenheitSensorAdapter(legacy)
    println(s"legacy=${legacy.readFahrenheit()}F")
    println(s"adapted=${reader.readCelsius()}C")
  }
}
