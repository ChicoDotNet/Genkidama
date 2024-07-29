' Abstract Factory
Function CreateReport(reportType As String) As Object
    If reportType = "pdf" Then
        Set CreateReport = New PDFReport
    ElseIf reportType = "html" Then
        Set CreateReport = New HTMLReport
    End If
End Function

' Concrete Implementations
Class PDFReport
    Sub Generate()
        MsgBox "Generating PDF report"
    End Sub
End Class

Class HTMLReport
    Sub Generate()
        MsgBox "Generating HTML report"
    End Sub
End Class

Sub Usage()
    Dim report As Object
    Set report = CreateReport("pdf")
    report.Generate

    Set report = CreateReport("html")
    report.Generate
End Sub
