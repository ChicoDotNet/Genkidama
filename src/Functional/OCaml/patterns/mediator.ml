let () =
  let events = ref [] in
  let mediate sender message =
    events := (sender ^ ":" ^ message) :: !events
  in
  mediate "checkout" "paid";
  assert (!events = [ "checkout:paid" ])
