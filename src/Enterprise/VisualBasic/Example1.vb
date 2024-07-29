Imports System

' Abstract Product
Public Interface Button
    Sub Render()
End Interface

Public Interface Checkbox
    Sub Render()
End Interface

' Concrete Product
Public Class DarkButton
    Implements Button
    Public Sub Render() Implements Button.Render
        Console.WriteLine("Dark Button")
    End Sub
End Class

Public Class LightButton
    Implements Button
    Public Sub Render() Implements Button.Render
        Console.WriteLine("Light Button")
    End Sub
End Class

Public Class DarkCheckbox
    Implements Checkbox
    Public Sub Render() Implements Checkbox.Render
        Console.WriteLine("Dark Checkbox")
    End Sub
End Class

Public Class LightCheckbox
    Implements Checkbox
    Public Sub Render() Implements Checkbox.Render
        Console.WriteLine("Light Checkbox")
    End Sub
End Class

' Abstract Factory
Public Interface UIFactory
    Function CreateButton() As Button
    Function CreateCheckbox() As Checkbox
End Interface

' Concrete Factory
Public Class DarkFactory
    Implements UIFactory
    Public Function CreateButton() As Button Implements UIFactory.CreateButton
        Return New DarkButton()
    End Function
    Public Function CreateCheckbox() As Checkbox Implements UIFactory.CreateCheckbox
        Return New DarkCheckbox()
    End Function
End Class

Public Class LightFactory
    Implements UIFactory
    Public Function CreateButton() As Button Implements UIFactory.CreateButton
        Return New LightButton()
    End Function
    Public Function CreateCheckbox() As Checkbox Implements UIFactory.CreateCheckbox
        Return New LightCheckbox()
    End Function
End Class

Module Example1
    Sub CreateUIComponents(factory As UIFactory)
        Dim button As Button = factory.CreateButton()
        Dim checkbox As Checkbox = factory.CreateCheckbox()
        button.Render()
        checkbox.Render()
    End Sub

    Sub Main()
        CreateUIComponents(New DarkFactory())
        CreateUIComponents(New LightFactory())
    End Sub
End Module
