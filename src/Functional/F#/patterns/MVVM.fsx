module MvvmExample
let run ()=let mutable amount=10 in let text()=sprintf "$%d.00" amount in let before=text() in amount<-amount+5;before="$10.00"&&text()="$15.00"
