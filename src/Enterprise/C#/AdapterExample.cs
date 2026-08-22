using System;

public interface ITemperatureReader
{
    int ReadCelsius();
}

public sealed class LegacyFahrenheitSensor
{
    public int ReadFahrenheit() => 86;
}

public sealed class FahrenheitSensorAdapter : ITemperatureReader
{
    private readonly LegacyFahrenheitSensor _sensor;

    public FahrenheitSensorAdapter(LegacyFahrenheitSensor sensor) => _sensor = sensor;

    public int ReadCelsius() => (_sensor.ReadFahrenheit() - 32) * 5 / 9;
}

public static class AdapterExample
{
    public static void Main()
    {
        var legacy = new LegacyFahrenheitSensor();
        ITemperatureReader reader = new FahrenheitSensorAdapter(legacy);
        Console.WriteLine($"legacy={legacy.ReadFahrenheit()}F");
        Console.WriteLine($"adapted={reader.ReadCelsius()}C");
    }
}
