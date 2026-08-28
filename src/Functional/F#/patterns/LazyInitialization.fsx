module LazyInitializationExample
let run () =
    let mutable builds = 0
    let value =
        lazy (
            builds <- builds + 1
            "ready"
        )
    value.Value = "ready" && value.Value = "ready" && builds = 1
