Imports System

Public Interface IComponent
    Function Render() As String
End Interface

Public NotInheritable Class PlainMessage
    Implements IComponent

    Public Function Render() As String Implements IComponent.Render
        Return "alert"
    End Function
End Class

Public MustInherit Class ComponentDecorator
    Implements IComponent

    Protected ReadOnly Inner As IComponent

    Protected Sub New(innerComponent As IComponent)
        Inner = innerComponent
    End Sub

    Public MustOverride Function Render() As String Implements IComponent.Render
End Class

Public NotInheritable Class AuditDecorator
    Inherits ComponentDecorator

    Public Sub New(innerComponent As IComponent)
        MyBase.New(innerComponent)
    End Sub

    Public Overrides Function Render() As String
        Return $"audit({Inner.Render()})"
    End Function
End Class

Public NotInheritable Class EncryptDecorator
    Inherits ComponentDecorator

    Public Sub New(innerComponent As IComponent)
        MyBase.New(innerComponent)
    End Sub

    Public Overrides Function Render() As String
        Return $"enc({Inner.Render()})"
    End Function
End Class

Module DecoratorExample
    Sub Main()
        Dim component As IComponent = New PlainMessage()
        Console.WriteLine($"base={component.Render()}")
        Console.WriteLine($"audit={New AuditDecorator(component).Render()}")
        Console.WriteLine($"encrypted={New EncryptDecorator(component).Render()}")
        Console.WriteLine($"stacked={New AuditDecorator(New EncryptDecorator(component)).Render()}")
    End Sub
End Module
