let render body = "<" ^ body () ^ ">"

let () = assert (render (fun () -> "sales") = "<sales>")
