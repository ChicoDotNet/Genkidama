Imports System

Public Interface ITemperatureReader
    Function ReadCelsius() As Integer
End Interface

Public NotInheritable Class LegacyFahrenheitSensor
    Public Function ReadFahrenheit() As Integer
        Return 86
    End Function
End Class

Public NotInheritable Class FahrenheitSensorAdapter
    Implements ITemperatureReader

    Private ReadOnly _legacy As LegacyFahrenheitSensor

    Public Sub New(legacy As LegacyFahrenheitSensor)
        _legacy = legacy
    End Sub

    Public Function ReadCelsius() As Integer Implements ITemperatureReader.ReadCelsius
        Return CInt(Math.Round((_legacy.ReadFahrenheit() - 32) * 5.0 / 9.0))
    End Function
End Class

Module AdapterExample
    Sub Main()
        Dim legacy = New LegacyFahrenheitSensor()
        Dim reader As ITemperatureReader = New FahrenheitSensorAdapter(legacy)
        Console.WriteLine($"legacy={legacy.ReadFahrenheit()}F")
        Console.WriteLine($"adapted={reader.ReadCelsius()}C")
    End Sub
End Module
