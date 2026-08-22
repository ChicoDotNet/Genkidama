Imports System
Imports System.Collections.Generic

Public Class ServiceProfile
    Public Property Name As String
    Public Property Features As List(Of String)

    Public Sub New(name As String, features As IEnumerable(Of String))
        Me.Name = name
        Me.Features = New List(Of String)(features)
    End Sub

    Public Function CloneProfile() As ServiceProfile
        Return New ServiceProfile(Name, Features)
    End Function

    Public Function Describe() As String
        Return $"{Name}: {String.Join(",", Features)}"
    End Function
End Class

Module PrototypeExample
    Sub Main()
        Dim original = New ServiceProfile("orders", {"metrics"})
        Dim canary = original.CloneProfile()
        canary.Name = "orders-canary"
        canary.Features.Add("tracing")

        Console.WriteLine($"original={original.Describe()}")
        Console.WriteLine($"clone={canary.Describe()}")
    End Sub
End Module
