Imports System
Friend Module MvvmExample
    Friend Function Run() As Boolean
        Dim amount=10
        Dim text As Func(Of String)=Function() $"${amount}.00"
        Dim before=text()
        amount+=5
        Return before="$10.00" AndAlso text()="$15.00"
    End Function
End Module
