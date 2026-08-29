let () =
  let workers = [ "leader"; "follower" ] in
  let events = [ "one"; "two" ] in
  let handled =
    List.map2 (fun worker event -> worker ^ ":" ^ event) workers events
  in
  assert (handled = [ "leader:one"; "follower:two" ])
