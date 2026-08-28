Imports System.Collections.Generic
Friend Module ObjectPoolExample
    Friend Function Run() As Boolean
        Dim pool As New Stack(Of Integer)(New Integer(){1,2})
        Dim x=pool.Pop()
        pool.Push(x)
        Return pool.Count=2 AndAlso pool.Contains(x)
    End Function
End Module
