let stock () = true
let charge () = "paid"

let checkout () =
  if stock () then charge () else "sold_out"

let () = assert (checkout () = "paid")
