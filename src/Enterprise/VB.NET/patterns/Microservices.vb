Imports System
Friend Module MicroservicesExample
    Friend Function Run() As Boolean
        Dim stock=7
        Dim reserve As Func(Of Integer,Boolean)=Function(q)
            If q>stock Then Return False
            stock-=q
            Return True
        End Function
        Return reserve(2) AndAlso stock=5
    End Function
End Module
