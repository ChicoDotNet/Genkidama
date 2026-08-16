' Builder — procedural VBA representation
Option Explicit

Private Enum ReportFormat
    TextReport = 1
    HtmlReport = 2
End Enum

Private Type ReportBuilder
    Format As ReportFormat
    Parts As Collection
End Type

Private Function CreateBuilder(ByVal format As ReportFormat) As ReportBuilder
    Dim target As ReportBuilder
    target.Format = format
    Set target.Parts = New Collection
    CreateBuilder = target
End Function

Private Sub Reset(ByRef target As ReportBuilder)
    Set target.Parts = New Collection
End Sub

Private Sub AddTitle(ByRef target As ReportBuilder, ByVal title As String)
    If target.Format = TextReport Then
        target.Parts.Add "# " & title
    Else
        target.Parts.Add "<h1>" & title & "</h1>"
    End If
End Sub

Private Sub AddSection(ByRef target As ReportBuilder, ByVal heading As String, ByVal body As String)
    If target.Format = TextReport Then
        target.Parts.Add "## " & heading
        target.Parts.Add body
    Else
        target.Parts.Add "<h2>" & heading & "</h2><p>" & body & "</p>"
    End If
End Sub

Private Function Build(ByRef target As ReportBuilder) As String
    Dim item As Variant
    Dim result As String
    For Each item In target.Parts
        If Len(result) > 0 Then result = result & vbCrLf
        result = result & CStr(item)
    Next item
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
