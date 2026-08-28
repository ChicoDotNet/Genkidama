function strategyPattern(){const price=(v:number,s:(x:number)=>number)=>s(v);return price(100,x=>x)===100&&price(100,x=>x*0.8)===80}
