function unitOfWorkPattern(){const store:number[]=[],pending=[2,3];store.push(...pending);pending.length=0;return store.join(',')==='2,3'&&pending.length===0}
