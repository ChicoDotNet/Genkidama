' Abstract Factory — procedural VBA representation
Option Explicit

Private Enum ThemeFamily
    DarkTheme = 1
    LightTheme = 2
End Enum

Private Type UIFactory
    Family As ThemeFamily
End Type

Private Function CreateFactory(ByVal family As ThemeFamily) As UIFactory
    Dim factory As UIFactory
    factory.Family = family
    CreateFactory = factory
End Function

Private Function CreateButton(ByRef factory As UIFactory) As String
    Select Case factory.Family
        Case DarkTheme
            CreateButton = "Dark Button"
        Case LightTheme
            CreateButton = "Light Button"
        Case Else
            Err.Raise vbObjectError + 1, "AbstractFactory", "Unknown theme family"
    End Select
End Function

Private Function CreateCheckbox(ByRef factory As UIFactory) As String
    Select Case factory.Family
        Case DarkTheme
            CreateCheckbox = "Dark Checkbox"
        Case LightTheme
            CreateCheckbox = "Light Checkbox"
        Case Else
            Err.Raise vbObjectError + 1, "AbstractFactory", "Unknown theme family"
    End Select
End Function

Public Sub Usage()
    Dim factory As UIFactory
    factory = CreateFactory(DarkTheme)

    Debug.Print CreateButton(factory)
    Debug.Print CreateCheckbox(factory)

    factory = CreateFactory(LightTheme)

    Debug.Print CreateButton(factory)
    Debug.Print CreateCheckbox(factory)
End Sub
