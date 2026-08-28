Imports System.Collections.Generic
Imports System.Linq
Friend Module IteratorExample
    Friend Function Run() As Boolean
        Dim values={10,20,30},seen As New List(Of Integer)
        For Each value In values : seen.Add(value) : Next
        Return seen.SequenceEqual(values)
    End Function
End Module
