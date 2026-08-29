let () =
  let document = "One" in
  let plain_view () = document in
  let upper_view () = String.uppercase_ascii document in
  assert (plain_view () = "One" && upper_view () = "ONE")
