Imports System
Imports System.Collections.Generic
Friend Module MicrokernelExample
    Friend Function Run() As Boolean
        Dim plugins As New Dictionary(Of String,Func(Of Integer,Integer)) From {{"double",Function(x)x*2},{"square",Function(x)x*x}}
        Return plugins("double")(4)=8 AndAlso plugins("square")(4)=16
    End Function
End Module
