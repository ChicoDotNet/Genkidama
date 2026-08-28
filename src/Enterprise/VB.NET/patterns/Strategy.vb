Imports System
Friend Module StrategyExample
    Friend Function Run() As Boolean
        Dim price As Func(Of Integer,Func(Of Integer,Integer),Integer)=Function(v,s) s(v)
        Return price(100,Function(x)x)=100 AndAlso price(100,Function(x)x*80\100)=80
    End Function
End Module
