module MvcExample
let run ()=let mutable count=0 in let view()=$"count={count}" in let before=view() in count<-count+1;before="count=0"&&view()="count=1"
