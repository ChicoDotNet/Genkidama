Imports System.Collections.Generic
Friend Module ActiveRecordExample
    Friend Function Run() As Boolean
        Dim table As New Dictionary(Of Integer,String) From {{7,"Ada"}}
        Return table(7)="Ada"
    End Function
End Module
