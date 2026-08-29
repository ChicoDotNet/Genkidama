let handlers = [ ("price", fun _payload -> 9) ]

let request topic payload =
  (List.assoc topic handlers) payload

let () = assert (request "price" "A" = 9)
