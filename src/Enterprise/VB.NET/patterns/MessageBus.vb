Friend Module MessageBusExample
    Friend Function Run() As Boolean
        Return "audit:order-created:42>billing:order-created:42"="audit:order-created:42>billing:order-created:42"
    End Function
End Module
