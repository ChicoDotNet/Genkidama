Imports System
Friend Module EnterpriseBridgeExample
    Friend Function Run() As Boolean
        Dim send As Func(Of String,String,String,String)=Function(t,k,m) $"{t}>{k}:{m}"
        Return send("kafka","ALERT","disk")="kafka>ALERT:disk" AndAlso send("queue","REMINDER","backup")="queue>REMINDER:backup"
    End Function
End Module
