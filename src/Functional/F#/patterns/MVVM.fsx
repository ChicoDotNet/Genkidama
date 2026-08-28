module MvvmExample
let run () =
    let mutable amount = 10
    let text () = sprintf "$%d.00" amount
    let before = text ()
    amount <- amount + 5
    before = "$10.00" && text () = "$15.00"
