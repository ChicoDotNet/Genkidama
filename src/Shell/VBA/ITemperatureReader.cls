Option Explicit

Public Function ReadCelsius() As Long
    Err.Raise vbObjectError + 1000, "ITemperatureReader", "Interface member only"
End Function
