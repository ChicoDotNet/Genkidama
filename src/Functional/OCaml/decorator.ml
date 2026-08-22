type component = unit -> string

let base_component () = "alert"
let audit_decorator (component : component) () = "audit(" ^ component () ^ ")"
let encrypt_decorator (component : component) () = "enc(" ^ component () ^ ")"

let () =
  let base = base_component in
  let audited = audit_decorator base in
  let encrypted = encrypt_decorator base in
  let stacked = audit_decorator (encrypt_decorator base) in
  Printf.printf "base=%s\n" (base ());
  Printf.printf "audit=%s\n" (audited ());
  Printf.printf "encrypted=%s\n" (encrypted ());
  Printf.printf "stacked=%s\n" (stacked ())
