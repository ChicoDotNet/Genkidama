function objectPoolPattern(){const pool=[1,2];const x=pool.pop()!;pool.push(x);return pool.length===2&&pool.includes(x)}
