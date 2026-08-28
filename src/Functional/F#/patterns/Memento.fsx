module MementoExample
let run ()=let mutable state="draft" in let snapshot=state in state<-"published";state<-snapshot;state="draft"
