module MessageBusExample
let run () =
    let handlers =
        [
            (fun topic id -> $"audit:{topic}:{id}")
            (fun topic id -> $"billing:{topic}:{id}")
        ]
    handlers
    |> List.map (fun handler -> handler "order-created" 42)
    |> String.concat ">"
    |> (=) "audit:order-created:42>billing:order-created:42"
