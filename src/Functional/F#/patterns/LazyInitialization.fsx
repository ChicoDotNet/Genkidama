module LazyInitializationExample
let run ()=let mutable builds=0 in let value=lazy(builds<-builds+1;"ready") in value.Value="ready"&&value.Value="ready"&&builds=1
