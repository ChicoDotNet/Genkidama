Friend Module MonitorObjectExample
    Friend Function Run() As Boolean
        Dim gate As New Object(),value=0
        SyncLock gate
            value+=2
        End SyncLock
        SyncLock gate
            value+=3
        End SyncLock
        Return value=5
    End Function
End Module
