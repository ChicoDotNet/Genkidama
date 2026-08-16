Imports System
Imports System.Collections.Generic

Public Interface IReportBuilder
    Sub Reset()
    Sub AddTitle(title As String)
    Sub AddSection(heading As String, body As String)
    Function Build() As String
End Interface

Public Class TextReportBuilder
    Implements IReportBuilder
    Private ReadOnly parts As New List(Of String)()

    Public Sub Reset() Implements IReportBuilder.Reset
        parts.Clear()
    End Sub

    Public Sub AddTitle(title As String) Implements IReportBuilder.AddTitle
        parts.Add("# " & title)
    End Sub

    Public Sub AddSection(heading As String, body As String) Implements IReportBuilder.AddSection
        parts.Add("## " & heading)
        parts.Add(body)
    End Sub

    Public Function Build() As String Implements IReportBuilder.Build
        Return String.Join(Environment.NewLine, parts)
    End Function
End Class

Public Class HtmlReportBuilder
    Implements IReportBuilder
    Private ReadOnly parts As New List(Of String)()

    Public Sub Reset() Implements IReportBuilder.Reset
        parts.Clear()
    End Sub

    Public Sub AddTitle(title As String) Implements IReportBuilder.AddTitle
        parts.Add("<h1>" & title & "</h1>")
    End Sub

    Public Sub AddSection(heading As String, body As String) Implements IReportBuilder.AddSection
        parts.Add("<h2>" & heading & "</h2>")
        parts.Add("<p>" & body & "</p>")
    End Sub

    Public Function Build() As String Implements IReportBuilder.Build
        Return String.Concat(parts)
    End Function
End Class

Public Module BuilderExample
    Private Function BuildAvailabilityReport(builder As IReportBuilder) As String
        builder.Reset()
        builder.AddTitle("Service status")
        builder.AddSection("Availability", "99.95%")
        Return builder.Build()
    End Function

    Public Sub Main()
        Console.WriteLine(BuildAvailabilityReport(New TextReportBuilder()))
        Console.WriteLine("---")
        Console.WriteLine(BuildAvailabilityReport(New HtmlReportBuilder()))
    End Sub
End Module
