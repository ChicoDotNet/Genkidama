Imports System
Friend Module DependencyInjectionExample
    Friend Function Run() As Boolean
        Dim service As Func(Of Func(Of String),String)=Function(clock)$"at:{clock()}"
        Return service(Function()"10:00")="at:10:00"
    End Function
End Module
