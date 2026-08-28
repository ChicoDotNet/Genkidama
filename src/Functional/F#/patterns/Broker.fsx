module BrokerExample
let run () =
    let services =
        dict [
            "inventory", (fun key -> $"inventory:{key}=7")
            "customer", (fun key -> $"customer:{key}=active")
        ]
    services["inventory"] "sku-1" = "inventory:sku-1=7"
    && services["customer"] "17" = "customer:17=active"
