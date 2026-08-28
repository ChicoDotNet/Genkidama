module PublishSubscribeExample
let run () =
    let subscribers =
        [
            (fun id -> $"warehouse:{id}")
            (fun id -> $"analytics:{id}")
        ]
    subscribers
    |> List.map (fun subscriber -> subscriber 51)
    |> String.concat ">"
    |> (=) "warehouse:51>analytics:51"
