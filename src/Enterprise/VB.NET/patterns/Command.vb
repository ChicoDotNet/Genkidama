Imports System
Friend Module CommandExample
    Friend Function Run() As Boolean
        Dim q As Func(Of Integer,Integer)()={Function(x) x+50,Function(x) x-20}
        Dim balance=100
        For Each operation In q
            balance=operation(balance)
        Next
        Return balance=130 AndAlso q(1)(150)=130
    End Function
End Module
