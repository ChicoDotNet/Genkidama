<?php

declare(strict_types=1);

interface TemperatureReader
{
    public function readCelsius(): int;
}

final class LegacyFahrenheitSensor
{
    public function readFahrenheit(): int
    {
        return 86;
    }
}

final class FahrenheitSensorAdapter implements TemperatureReader
{
    public function __construct(private readonly LegacyFahrenheitSensor $sensor)
    {
    }

    public function readCelsius(): int
    {
        return intdiv(($this->sensor->readFahrenheit() - 32) * 5, 9);
    }
}

$legacy = new LegacyFahrenheitSensor();
$reader = new FahrenheitSensorAdapter($legacy);
printf("legacy=%dF\n", $legacy->readFahrenheit());
printf("adapted=%dC\n", $reader->readCelsius());
