function commandPattern(){const q=[(x:number)=>x+50,(x:number)=>x-20];const balance=q.reduce((v,f)=>f(v),100);return balance===130&&q[1](150)===130}
