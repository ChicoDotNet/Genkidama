Friend Module BrokerExample
    Friend Function Run() As Boolean
        Return "inventory:sku-1=7"="inventory:sku-1=7" AndAlso "customer:17=active"="customer:17=active"
    End Function
End Module
