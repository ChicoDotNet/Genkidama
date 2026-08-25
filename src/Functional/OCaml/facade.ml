let auth_service user = "auth(" ^ user ^ ")"
let inventory_service sku = "reserve(" ^ sku ^ ")"
let billing_service amount = "charge(" ^ string_of_int amount ^ ")"

let checkout_facade user sku amount =
  String.concat ">"
    [ auth_service user; inventory_service sku; billing_service amount ]

let () =
  Printf.printf "checkout=%s\n" (checkout_facade "alice" "SKU-42" 499)
