let ()=let state=ref[] in let mailbox=[fun()->state:="done"::!state] in List.iter(fun f->f())mailbox;assert(!state=["done"])
