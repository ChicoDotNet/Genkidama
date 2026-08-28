module ObserverExample
let run () =
    let observers = [ (fun id -> $"audit:{id}"); (fun id -> $"dashboard:{id}") ]
    observers
    |> List.map (fun observer -> observer 42)
    |> String.concat ">"
    |> (=) "audit:42>dashboard:42"
