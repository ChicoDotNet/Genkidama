' Abstract Factory
Function CreateButton(theme As String) As Object
    If theme = "dark" Then
        Set CreateButton = New DarkButton
    ElseIf theme = "light" Then
        Set CreateButton = New LightButton
    End If
End Function

Function CreateCheckbox(theme As String) As Object
    If theme = "dark" Then
        Set CreateCheckbox = New DarkCheckbox
    ElseIf theme = "light" Then
        Set CreateCheckbox = New LightCheckbox
    End If
End Function

' Concrete Products
Class DarkButton
    Sub Render()
        MsgBox "Dark Button"
    End Sub
End Class

Class LightButton
    Sub Render()
        MsgBox "Light Button"
    End Sub
End Class

Class DarkCheckbox
    Sub Render()
        MsgBox "Dark Checkbox"
    End Sub
End Class

Class LightCheckbox
    Sub Render()
        MsgBox "Light Checkbox"
    End Sub
End Class

Sub Usage()
    Dim button As Object
    Set button = CreateButton("dark")
    button.Render

    Dim checkbox As Object
    Set checkbox = CreateCheckbox("light")
    checkbox.Render
End Sub
