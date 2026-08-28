module ObserverExample
let run () =
    [ (fun i -> $"audit:{i}"); (fun i -> $"dashboard:{i}") ]
    |> List.map (fun observer -> observer 42)
    |> String.concat ">"
    |> (=) "audit:42>dashboard:42"
