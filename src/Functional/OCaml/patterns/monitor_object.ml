let () =
  let counter = ref 0 in
  let guarded_increment () = incr counter in
  guarded_increment ();
  assert (!counter = 1)
