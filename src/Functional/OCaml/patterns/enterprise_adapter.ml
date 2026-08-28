let legacy cents = cents

let adapter amount =
  legacy (int_of_float ((amount *. 100.0) +. 0.5))

let () = assert (adapter 12.34 = 1234)
