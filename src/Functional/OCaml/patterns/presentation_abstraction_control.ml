let () =
  let abstraction = ref 1 in
  let control delta = abstraction := !abstraction + delta in
  let presentation () = string_of_int !abstraction in
  control 2;
  assert (presentation () = "3")
