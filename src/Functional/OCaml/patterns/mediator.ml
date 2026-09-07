exception Unknown_colleague of string

type receiver = sender:string -> message:string -> string

type mediator = (string, receiver) Hashtbl.t

let register mediator name receiver = Hashtbl.replace mediator name receiver

let send mediator ~sender ~recipient ~message =
  match Hashtbl.find_opt mediator recipient with
  | Some receiver -> receiver ~sender ~message
  | None -> raise (Unknown_colleague recipient)

let () =
  let mediator = Hashtbl.create 2 in
  register mediator "payment" (fun ~sender ~message ->
      Printf.sprintf "payment received %s from %s" message sender);
  register mediator "inventory" (fun ~sender ~message ->
      Printf.sprintf "inventory received %s from %s" message sender);

  assert (
    send mediator ~sender:"payment" ~recipient:"inventory" ~message:"paid"
    = "inventory received paid from payment");
  assert (
    send mediator ~sender:"inventory" ~recipient:"payment" ~message:"reserved"
    = "payment received reserved from inventory");

  let rejected_unknown =
    try
      ignore (send mediator ~sender:"payment" ~recipient:"shipping" ~message:"paid");
      false
    with Unknown_colleague "shipping" -> true
  in
  assert rejected_unknown
