Option Explicit

Implements ITemperatureReader

Private adaptee As LegacyFahrenheitSensor

Public Sub Initialize(ByVal sensor As LegacyFahrenheitSensor)
    Set adaptee = sensor
End Sub

Private Function ITemperatureReader_ReadCelsius() As Long
    Dim fahrenheit As Long
    fahrenheit = adaptee.ReadFahrenheit()
    ITemperatureReader_ReadCelsius = ((fahrenheit - 32) * 5) \ 9
End Function
