let ()=let received=ref[] in let topics=[fun v->received:=v::!received]in List.iter(fun f->f"v1")topics;assert(!received=["v1"])
