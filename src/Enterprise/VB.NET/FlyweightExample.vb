Imports System
Imports System.Collections.Generic

Public NotInheritable Class TextStyle
    Public ReadOnly Property Font As String
    Public ReadOnly Property Size As Integer
    Public ReadOnly Property Color As String

    Public Sub New(font As String, size As Integer, color As String)
        Me.Font = font
        Me.Size = size
        Me.Color = color
    End Sub
End Class

Public NotInheritable Class StyleFactory
    Private ReadOnly styles As New Dictionary(Of String, TextStyle)()

    Public Function [Get](font As String, size As Integer, color As String) As TextStyle
        Dim key = $"{font}|{size}|{color}"
        Dim style As TextStyle = Nothing
        If Not styles.TryGetValue(key, style) Then
            style = New TextStyle(font, size, color)
            styles(key) = style
        End If
        Return style
    End Function

    Public ReadOnly Property Count As Integer
        Get
            Return styles.Count
        End Get
    End Property
End Class

Module Program
    Sub Main()
        Dim factory = New StyleFactory()
        Dim red1 = factory.Get("Inter", 12, "red")
        Dim red2 = factory.Get("Inter", 12, "red")
        factory.Get("Inter", 12, "blue")
        Console.WriteLine($"styles={factory.Count};shared={Object.ReferenceEquals(red1, red2).ToString().ToLowerInvariant()};text=ABC")
    End Sub
End Module
