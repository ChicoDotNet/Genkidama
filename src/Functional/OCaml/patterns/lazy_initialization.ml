let ()=let calls=ref 0 and value=ref None in let get()=match!value with Some v->v|None->incr calls;value:=Some 7;7 in ignore(get());ignore(get());assert(!calls=1)
