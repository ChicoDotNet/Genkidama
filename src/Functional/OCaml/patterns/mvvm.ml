let () =
  let model = ("Ada", "Lovelace") in
  let view_model (first, last) = first ^ " " ^ last in
  assert (view_model model = "Ada Lovelace")
