module MvcExample
let run () =
    let mutable count = 0
    let view () = $"count={count}"
    let before = view ()
    count <- count + 1
    before = "count=0" && view () = "count=1"
