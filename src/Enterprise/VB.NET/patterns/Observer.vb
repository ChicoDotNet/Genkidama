Imports System
Imports System.Linq
Friend Module ObserverExample
    Friend Function Run() As Boolean
        Dim observers As Func(Of Integer,String)()={Function(i) $"audit:{i}",Function(i) $"dashboard:{i}"}
        Return String.Join(">",observers.Select(Function(o) o(42)))="audit:42>dashboard:42"
    End Function
End Module
