type ITemperatureReader =
    abstract member ReadCelsius: unit -> int

type LegacyFahrenheitSensor() =
    member _.ReadFahrenheit() = 86

type FahrenheitSensorAdapter(sensor: LegacyFahrenheitSensor) =
    interface ITemperatureReader with
        member _.ReadCelsius() = (sensor.ReadFahrenheit() - 32) * 5 / 9

let legacy = LegacyFahrenheitSensor()
let reader: ITemperatureReader = FahrenheitSensorAdapter(legacy)
printfn "legacy=%dF" (legacy.ReadFahrenheit())
printfn "adapted=%dC" (reader.ReadCelsius())
