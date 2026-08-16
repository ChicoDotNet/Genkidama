' Builder — procedural VBA representation
Option Explicit

Private Enum ReportFormat
    TextReport = 1
    HtmlReport = 2
End Enum

Private Type ReportBuilder
    Format As ReportFormat
    PartCount As Long
    Parts(1 To 3) As String
End Type

Private Function CreateBuilder(ByVal format As ReportFormat) As ReportBuilder
    Dim target As ReportBuilder
    target.Format = format
    CreateBuilder = target
End Function

Private Sub Reset(ByRef target As ReportBuilder)
    Dim index As Long
    target.PartCount = 0
    For index = LBound(target.Parts) To UBound(target.Parts)
        target.Parts(index) = vbNullString
    Next index
End Sub

Private Sub AppendPart(ByRef target As ReportBuilder, ByVal value As String)
    target.PartCount = target.PartCount + 1
    target.Parts(target.PartCount) = value
End Sub

Private Sub AddTitle(ByRef target As ReportBuilder, ByVal title As String)
    If target.Format = TextReport Then
        AppendPart target, "# " & title
    Else
        AppendPart target, "<h1>" & title & "</h1>"
    End If
End Sub

Private Sub AddSection(ByRef target As ReportBuilder, ByVal heading As String, ByVal body As String)
    If target.Format = TextReport Then
        AppendPart target, "## " & heading
        AppendPart target, body
    Else
        AppendPart target, "<h2>" & heading & "</h2><p>" & body & "</p>"
    End If
End Sub

Private Function Build(ByRef target As ReportBuilder) As String
    Dim index As Long
    Dim result As String
    For index = 1 To target.PartCount
        If Len(result) > 0 Then result = result & vbCrLf
        result = result & target.Parts(index)
    Next index
    Build = result
End Function

Private Function BuildAvailabilityReport(ByRef target As ReportBuilder) As String
    Reset target
    AddTitle target, "Service status"
    AddSection target, "Availability", "99.95%"
    BuildAvailabilityReport = Build(target)
End Function

Public Sub Usage()
    Dim textBuilder As ReportBuilder
    Dim htmlBuilder As ReportBuilder
    textBuilder = CreateBuilder(TextReport)
    htmlBuilder = CreateBuilder(HtmlReport)
    Debug.Print BuildAvailabilityReport(textBuilder)
    Debug.Print "---"
    Debug.Print BuildAvailabilityReport(htmlBuilder)
End Sub
