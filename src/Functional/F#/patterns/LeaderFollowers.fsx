module LeaderFollowersExample
let run () =
    let workers = [ "worker-1"; "worker-2"; "worker-3" ]
    let events = [ "a"; "b"; "c" ]
    let handled =
        events
        |> List.mapi (fun index event -> $"{workers[index % workers.Length]}:{event}")
    String.concat ">" handled = "worker-1:a>worker-2:b>worker-3:c"
    && workers[events.Length % workers.Length] = "worker-1"
