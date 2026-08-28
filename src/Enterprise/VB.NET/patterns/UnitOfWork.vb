Imports System.Collections.Generic
Imports System.Linq
Friend Module UnitOfWorkExample
    Friend Function Run() As Boolean
        Dim store As New List(Of Integer),pending As New List(Of Integer) From {2,3}
        store.AddRange(pending)
        pending.Clear()
        Return store.SequenceEqual({2,3}) AndAlso pending.Count=0
    End Function
End Module
