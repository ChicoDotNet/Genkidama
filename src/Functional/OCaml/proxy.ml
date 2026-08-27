type backend = { mutable created : int; mutable fetches : int }

let make_proxy backend =
  let cache = Hashtbl.create 4 in
  let subject_created = ref false in
  fun id ->
    match Hashtbl.find_opt cache id with
    | Some value -> value
    | None ->
        if not !subject_created then (
          backend.created <- backend.created + 1;
          subject_created := true);
        backend.fetches <- backend.fetches + 1;
        let value = Printf.sprintf "doc(%d)" id in
        Hashtbl.add cache id value;
        value

let () =
  let backend = { created = 0; fetches = 0 } in
  let get_document = make_proxy backend in
  let first = get_document 42 in
  let second = get_document 42 in
  Printf.printf "backend=%d;fetches=%d;first=%s;second=%s\n"
    backend.created backend.fetches first second
