Friend Module MementoExample
    Friend Function Run() As Boolean
        Dim state="draft",snapshot=state : state="published" : state=snapshot : Return state="draft"
    End Function
End Module
