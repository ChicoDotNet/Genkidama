let services = [ ("clock", fun () -> "12:00") ]

let () =
  let clock = List.assoc "clock" services in
  assert (clock () = "12:00")
