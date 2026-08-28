Friend Module ClientServerExample
    Friend Function Run() As Boolean
        Return Tuple.Create(200,"stock=7").Equals(Tuple.Create(200,"stock=7"))
    End Function
End Module
