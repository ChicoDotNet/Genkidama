let () =
  let state = ref [] in
  let mailbox = [ (fun () -> state := "done" :: !state) ] in
  List.iter (fun task -> task ()) mailbox;
  assert (!state = [ "done" ])
