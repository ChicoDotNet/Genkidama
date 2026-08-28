Friend Module DataMapperExample
    Friend Function Run() As Boolean
        Dim id=8,name="Grace",key=$"person:{id}"
        Return key="person:8" AndAlso name="Grace"
    End Function
End Module
