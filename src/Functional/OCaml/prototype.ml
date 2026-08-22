type service_profile = {
  name : string;
  features : string list;
}

let clone_profile profile =
  { profile with features = List.map Fun.id profile.features }

let describe profile =
  profile.name ^ ": " ^ String.concat "," profile.features

let () =
  let original = { name = "orders"; features = ["metrics"] } in
  let copy = clone_profile original in
  let canary =
    {
      name = "orders-canary";
      features = copy.features @ ["tracing"];
    }
  in
  Printf.printf "original=%s\n" (describe original);
  Printf.printf "clone=%s\n" (describe canary)
