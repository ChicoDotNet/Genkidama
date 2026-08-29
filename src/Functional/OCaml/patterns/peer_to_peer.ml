let send_direct source message = source ^ ":" ^ message

let () = assert (send_direct "a" "hello" = "a:hello")
