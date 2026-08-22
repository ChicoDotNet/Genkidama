package main

import "fmt"

type TemperatureReader interface {
	ReadCelsius() int
}

type LegacyFahrenheitSensor struct{}

func (LegacyFahrenheitSensor) ReadFahrenheit() int {
	return 86
}

type FahrenheitSensorAdapter struct {
	sensor LegacyFahrenheitSensor
}

func (adapter FahrenheitSensorAdapter) ReadCelsius() int {
	return (adapter.sensor.ReadFahrenheit() - 32) * 5 / 9
}

func main() {
	legacy := LegacyFahrenheitSensor{}
	var reader TemperatureReader = FahrenheitSensorAdapter{sensor: legacy}
	fmt.Printf("legacy=%dF\n", legacy.ReadFahrenheit())
	fmt.Printf("adapted=%dC\n", reader.ReadCelsius())
}
