let () =
  let received = ref [] in
  let topic = [ (fun value -> received := value :: !received) ] in
  List.iter (fun subscriber -> subscriber "v1") topic;
  assert (!received = [ "v1" ])
