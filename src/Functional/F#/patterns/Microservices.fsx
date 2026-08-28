module MicroservicesExample
let run () =
    let mutable stock = 7
    let reserve quantity =
        if quantity > stock then false
        else
            stock <- stock - quantity
            true
    let place quantity =
        if reserve quantity then "confirmed" else "rejected"
    place 2 = "confirmed" && stock = 5
