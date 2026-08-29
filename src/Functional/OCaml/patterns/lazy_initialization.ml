let () =
  let calls = ref 0 in
  let value = ref None in
  let get () =
    match !value with
    | Some current -> current
    | None ->
        incr calls;
        value := Some 7;
        7
  in
  ignore (get ());
  ignore (get ());
  assert (!calls = 1)
