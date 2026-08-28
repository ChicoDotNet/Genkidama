let () =
  let seen = ref [] in
  List.iter (fun value -> seen := value :: !seen) [ 3; 2; 1 ];
  assert (List.rev !seen = [ 3; 2; 1 ])
