Option Explicit

Public Sub RunAdapterExample()
    Dim legacy As LegacyFahrenheitSensor
    Dim concreteAdapter As FahrenheitSensorAdapter
    Dim reader As ITemperatureReader

    Set legacy = New LegacyFahrenheitSensor
    Set concreteAdapter = New FahrenheitSensorAdapter
    concreteAdapter.Initialize legacy
    Set reader = concreteAdapter

    Debug.Print "legacy=" & legacy.ReadFahrenheit() & "F"
    Debug.Print "adapted=" & reader.ReadCelsius() & "C"
End Sub
