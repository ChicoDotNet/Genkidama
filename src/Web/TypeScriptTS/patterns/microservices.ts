function microservicesPattern(){let stock=7;const reserve=(q:number)=>q>stock?false:(stock-=q,true);const place=(q:number)=>reserve(q)?'confirmed':'rejected';return place(2)==='confirmed'&&stock===5}
