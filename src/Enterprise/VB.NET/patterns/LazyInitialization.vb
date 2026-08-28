Imports System
Friend Module LazyInitializationExample
    Friend Function Run() As Boolean
        Dim builds=0,cache As String=Nothing
        Dim getValue As Func(Of String)=Function()
            If cache Is Nothing Then builds+=1 : cache="ready"
            Return cache
        End Function
        Return getValue()="ready" AndAlso getValue()="ready" AndAlso builds=1
    End Function
End Module
