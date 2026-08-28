Friend Module EnterpriseAdapterExample
    Friend Function Run() As Boolean
        Dim code=17,cents=1250,amount=cents/100.0
        Return code=17 AndAlso amount=12.5
    End Function
End Module
