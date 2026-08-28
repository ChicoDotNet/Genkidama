module DistributedProxyExample
let run ()=let remote _=7 in let proxy sku=remote sku in proxy "sku-1"=7
