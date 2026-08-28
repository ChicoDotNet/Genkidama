function distributedProxyPattern(){const remote=(sku:string)=>sku==='sku-1'?7:0;const proxy=(sku:string)=>remote(sku);return proxy('sku-1')===7}
