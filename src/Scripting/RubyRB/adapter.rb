class LegacyFahrenheitSensor
  def read_fahrenheit
    86
  end
end

class FahrenheitSensorAdapter
  def initialize(legacy)
    @legacy = legacy
  end

  def read_celsius
    ((@legacy.read_fahrenheit - 32) * 5.0 / 9.0).round
  end
end

legacy = LegacyFahrenheitSensor.new
reader = FahrenheitSensorAdapter.new(legacy)
puts "legacy=#{legacy.read_fahrenheit}F"
puts "adapted=#{reader.read_celsius}C"
