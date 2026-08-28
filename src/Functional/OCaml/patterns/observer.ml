let ()=let seen=ref[] in let subscribers=[fun e->seen:=e::!seen] in List.iter(fun f->f"changed")subscribers;assert(!seen=["changed"])
