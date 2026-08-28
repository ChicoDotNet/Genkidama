let ()=let seen=ref[] in List.iter(fun x->seen:=x::!seen)[3;2;1];assert(List.rev !seen=[3;2;1])
