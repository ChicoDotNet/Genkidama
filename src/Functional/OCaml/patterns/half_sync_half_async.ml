let () =
  let incoming = [ "a"; "b" ] in
  let completed = List.map String.uppercase_ascii incoming in
  assert (completed = [ "A"; "B" ])
