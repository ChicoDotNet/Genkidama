function lazyInitializationPattern(){let builds=0,cache:string|undefined;const get=()=>cache??=(builds++,'ready');return get()==='ready'&&get()==='ready'&&builds===1}
