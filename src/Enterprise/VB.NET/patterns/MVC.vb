Imports System
Friend Module MvcExample
    Friend Function Run() As Boolean
        Dim count=0
        Dim view As Func(Of String)=Function() $"count={count}"
        Dim before=view()
        count+=1
        Return before="count=0" AndAlso view()="count=1"
    End Function
End Module
