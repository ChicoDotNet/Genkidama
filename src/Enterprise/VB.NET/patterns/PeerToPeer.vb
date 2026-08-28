Friend Module PeerToPeerExample
    Friend Function Run() As Boolean
        Return "peer-a>peer-b:block-42>peer-a>peer-c:block-42"="peer-a>peer-b:block-42>peer-a>peer-c:block-42"
    End Function
End Module
