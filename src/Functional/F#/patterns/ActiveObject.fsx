module ActiveObjectExample
let run ()=let mutable value=0 in [(fun()->value<-value+3);(fun()->value<-value*4)]|>List.iter(fun action->action());value=12
