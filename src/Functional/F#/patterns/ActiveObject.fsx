module ActiveObjectExample
let run () =
    let mutable value = 0
    let queue =
        [
            (fun () -> value <- value + 3)
            (fun () -> value <- value * 4)
        ]
    let before = value
    queue |> List.iter (fun command -> command ())
    before = 0 && value = 12
