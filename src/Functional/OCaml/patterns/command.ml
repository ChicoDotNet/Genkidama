let () =
  let balance = ref 100 in
  let commands =
    [
      (fun () -> balance := !balance + 50);
      (fun () -> balance := !balance - 20);
    ]
  in
  List.iter (fun command -> command ()) commands;
  assert (!balance = 130)
