abstract class TemperatureReader
  abstract def read_celsius : Int32
end

class LegacyFahrenheitSensor
  def read_fahrenheit : Int32
    86
  end
end

class FahrenheitSensorAdapter < TemperatureReader
  def initialize(@adaptee : LegacyFahrenheitSensor)
  end

  def read_celsius : Int32
    ((@adaptee.read_fahrenheit - 32) * 5) // 9
  end
end

legacy = LegacyFahrenheitSensor.new
reader = FahrenheitSensorAdapter.new(legacy)

puts "legacy=#{legacy.read_fahrenheit}F"
puts "adapted=#{reader.read_celsius}C"
