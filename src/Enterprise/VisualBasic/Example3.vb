Imports System

' Abstract Product
Public Interface Report
    Sub Generate()
End Interface

' Concrete Product
Public Class PDFReport
    Implements Report
    Public Sub Generate() Implements Report.Generate
        Console.WriteLine("Generating PDF report")
    End Sub
End Class

Public Class HTMLReport
    Implements Report
    Public Sub Generate() Implements Report.Generate
        Console.WriteLine("Generating HTML report")
    End Sub
End Class

' Abstract Factory
Public Interface ReportFactory
    Function CreateReport() As Report
End Interface

' Concrete Factory
Public Class PDFReportFactory
    Implements ReportFactory
    Public Function CreateReport() As Report Implements ReportFactory.CreateReport
        Return New PDFReport()
    End Function
End Class

Public Class HTMLReportFactory
    Implements ReportFactory
    Public Function CreateReport() As Report Implements ReportFactory.CreateReport
        Return New HTMLReport()
    End Function
End Class

Module Example3
    Sub UseFactory(factory As ReportFactory)
        Dim report As Report = factory.CreateReport()
        report.Generate()
    End Sub

    Sub Main()
        UseFactory(New PDFReportFactory())
        UseFactory(New HTMLReportFactory())
    End Sub
End Module
