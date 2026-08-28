module DistributedProxyExample
let run () =
    let remote sku = if sku = "sku-1" then 7 else 0
    let proxy sku = remote sku
    proxy "sku-1" = 7
