let ()=let seen=ref[] in let bus=[fun v->seen:=v::!seen] in List.iter(fun f->f 42)bus;assert(!seen=[42])
