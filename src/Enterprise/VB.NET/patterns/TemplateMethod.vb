Imports System
Friend Module TemplateMethodExample
    Friend Function Run() As Boolean
        Dim pipeline As Func(Of String,Func(Of String),String)=Function(read,transform) $"{read}>{transform()}>publish"
        Return pipeline("read-csv",Function() "normalize")="read-csv>normalize>publish"
    End Function
End Module
