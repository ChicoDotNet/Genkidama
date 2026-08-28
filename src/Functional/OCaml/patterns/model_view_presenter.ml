let () =
  let model = "Ada" in
  let view = ref "" in
  let presenter () = view := String.uppercase_ascii model in
  presenter ();
  assert (!view = "ADA")
