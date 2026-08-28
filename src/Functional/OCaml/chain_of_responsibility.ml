type handler = {
  name : string;
  accepts : int -> bool;
}

let rec route amount visited = function
  | [] -> failwith "No handler accepted the request"
  | handler :: rest ->
      let visited_now = handler.name :: visited in
      if handler.accepts amount then
        (List.rev visited_now, handler.name)
      else
        route amount visited_now rest

let handlers = [
  { name = "faq"; accepts = (fun amount -> amount <= 50) };
  { name = "billing"; accepts = (fun amount -> amount <= 500) };
  { name = "escalation"; accepts = (fun _ -> true) };
]

let () =
  let amount = 250 in
  let visited, handled = route amount [] handlers in
  Printf.printf "visited=%s;handled=%s;result=refund(%d)\n"
    (String.concat ">" visited) handled amount
