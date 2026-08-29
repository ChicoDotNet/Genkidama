let () =
  let seen = ref [] in
  let subscribers = [ (fun event -> seen := event :: !seen) ] in
  List.iter (fun subscriber -> subscriber "changed") subscribers;
  assert (!seen = [ "changed" ])
