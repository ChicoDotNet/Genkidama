let legacy_read_fahrenheit () = 86

let adapt_to_celsius read_fahrenheit () =
  ((read_fahrenheit () - 32) * 5) / 9

let () =
  let read_celsius = adapt_to_celsius legacy_read_fahrenheit in
  Printf.printf "legacy=%dF\n" (legacy_read_fahrenheit ());
  Printf.printf "adapted=%dC\n" (read_celsius ())
