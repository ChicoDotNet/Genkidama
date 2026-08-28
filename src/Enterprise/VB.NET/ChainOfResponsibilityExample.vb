Imports System
Imports System.Collections.Generic

Friend NotInheritable Class Handler
    Private ReadOnly _name As String
    Private ReadOnly _limit As Integer
    Private _next As Handler

    Public Sub New(name As String, limit As Integer)
        _name = name
        _limit = limit
    End Sub

    Public Function ThenNext(handler As Handler) As Handler
        _next = handler
        Return handler
    End Function

    Public Function Handle(amount As Integer, visited As List(Of String)) As String
        visited.Add(_name)
        If amount <= _limit OrElse _next Is Nothing Then
            Return _name
        End If

        Return _next.Handle(amount, visited)
    End Function
End Class

Friend Module Program
    Public Sub Main()
        Dim faq = New Handler("faq", 50)
        Dim billing = New Handler("billing", 500)
        Dim escalation = New Handler("escalation", Integer.MaxValue)
        faq.ThenNext(billing).ThenNext(escalation)

        Dim visited As New List(Of String)()
        Dim handled = faq.Handle(250, visited)
        Console.WriteLine($"visited={String.Join(">", visited)};handled={handled};result=refund(250)")
    End Sub
End Module
