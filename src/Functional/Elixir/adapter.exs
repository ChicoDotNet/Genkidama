defmodule LegacyFahrenheitSensor do
  def read_fahrenheit, do: 86
end

defmodule FahrenheitSensorAdapter do
  def read_celsius(sensor_module) do
    fahrenheit = apply(sensor_module, :read_fahrenheit, [])
    div((fahrenheit - 32) * 5, 9)
  end
end

IO.puts("legacy=#{LegacyFahrenheitSensor.read_fahrenheit()}F")
IO.puts("adapted=#{FahrenheitSensorAdapter.read_celsius(LegacyFahrenheitSensor)}C")
