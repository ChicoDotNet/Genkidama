module MicroservicesExample
let run ()=let mutable stock=7 in let reserve q=if q>stock then false else stock<-stock-q;true in reserve 2&&stock=5
