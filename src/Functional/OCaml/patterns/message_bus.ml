let () =
  let seen = ref [] in
  let bus = [ (fun value -> seen := value :: !seen) ] in
  List.iter (fun handler -> handler 42) bus;
  assert (!seen = [ 42 ])
