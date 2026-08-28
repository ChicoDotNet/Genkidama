Imports System.Collections.Generic
Friend Module RepositoryExample
    Friend Function Run() As Boolean
        Dim rows As New Dictionary(Of Integer,String) From {{1,"Ada"},{2,"Grace"}}
        Return rows(2)="Grace"
    End Function
End Module
