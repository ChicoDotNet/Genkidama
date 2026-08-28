module ModelViewPresenterExample
let run () =
    let mutable count = 0
    let mutable text = ""
    let present () =
        count <- count + 1
        text <- $"count={count}"
    present ()
    count = 1 && text = "count=1"
