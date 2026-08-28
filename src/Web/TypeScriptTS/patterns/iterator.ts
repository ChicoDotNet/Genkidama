function iteratorPattern(){const it=[10,20,30][Symbol.iterator]();const seen=[it.next().value,it.next().value,it.next().value];return seen.join(',')==='10,20,30'&&it.next().done===true}
