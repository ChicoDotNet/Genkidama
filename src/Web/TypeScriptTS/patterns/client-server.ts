function clientServerPattern(){const server=(k:string)=>k==='sku-1'?{status:200,body:'stock=7'}:{status:404,body:'missing'};const r=server('sku-1');return r.status===200&&r.body==='stock=7'}
