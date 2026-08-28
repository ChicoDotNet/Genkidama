Imports System
Friend Module NullObjectExample
    Friend Function Run() As Boolean
        Dim nullLog As Func(Of String,String)=Function(m)""
        Dim realLog As Func(Of String,String)=Function(m)$"log:{m}"
        Return nullLog("x")="" AndAlso realLog("x")="log:x"
    End Function
End Module
