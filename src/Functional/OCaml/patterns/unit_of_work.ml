let () =
  let pending = ref [ 1 ] in
  let database = ref [] in
  database := !pending;
  pending := [];
  assert (!database = [ 1 ] && !pending = [])
