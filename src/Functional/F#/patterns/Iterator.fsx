module IteratorExample
let run () =
    let values = [ 10; 20; 30 ]
    let seen = values |> Seq.toList
    seen = values
