function observerPattern(){const observers=[(id:number)=>`audit:${id}`,(id:number)=>`dashboard:${id}`];return observers.map(o=>o(42)).join('>')==='audit:42>dashboard:42'}
