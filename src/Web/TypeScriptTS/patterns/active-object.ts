function activeObjectPattern(){let value=0;const queue=[()=>value+=3,()=>value*=4];const before=value;queue.forEach(f=>f());return before===0&&value===12}
