function publishSubscribePattern(){const subs=[(id:number)=>`warehouse:${id}`,(id:number)=>`analytics:${id}`];return subs.map(s=>s(51)).join('>')==='warehouse:51>analytics:51'}
