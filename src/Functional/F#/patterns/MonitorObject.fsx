module MonitorObjectExample
let run ()=let gate=obj() in let mutable value=0 in lock gate(fun()->value<-value+2);lock gate(fun()->value<-value+3);value=5
