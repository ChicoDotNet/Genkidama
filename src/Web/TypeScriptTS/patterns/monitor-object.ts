function monitorObjectPattern(){class Counter{#value=0;add(x:number){this.#value+=x}get value(){return this.#value}}const c=new Counter();c.add(2);c.add(3);return c.value===5}
