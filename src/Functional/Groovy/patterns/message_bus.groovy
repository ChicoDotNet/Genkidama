def seen=[];def bus=[paid:[{v->seen<<v}]];bus.paid*.call(42);assert seen==[42]
