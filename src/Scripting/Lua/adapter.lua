local legacy = {
  read_fahrenheit = function()
    return 86
  end
}

local function adapt_temperature(sensor)
  return {
    read_celsius = function()
      return math.floor(((sensor.read_fahrenheit() - 32) * 5 / 9) + 0.5)
    end
  }
end

local reader = adapt_temperature(legacy)
print(string.format("legacy=%dF", legacy.read_fahrenheit()))
print(string.format("adapted=%dC", reader.read_celsius()))
