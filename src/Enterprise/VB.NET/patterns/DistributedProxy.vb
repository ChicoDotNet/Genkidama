Imports System
Friend Module DistributedProxyExample
    Friend Function Run() As Boolean
        Dim remote As Func(Of String,Integer)=Function(sku) If(sku="sku-1",7,0)
        Dim proxy As Func(Of String,Integer)=Function(sku) remote(sku)
        Return proxy("sku-1")=7
    End Function
End Module
