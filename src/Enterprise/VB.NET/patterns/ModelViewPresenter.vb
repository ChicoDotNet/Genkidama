Friend Module ModelViewPresenterExample
    Friend Function Run() As Boolean
        Dim count=0,text=""
        count+=1
        text=$"count={count}"
        Return count=1 AndAlso text="count=1"
    End Function
End Module
