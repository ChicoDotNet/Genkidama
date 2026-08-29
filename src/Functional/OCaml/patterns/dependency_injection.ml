let greet clock = "hello@" ^ clock ()

let () =
  let clock () = "noon" in
  assert (greet clock = "hello@noon")
