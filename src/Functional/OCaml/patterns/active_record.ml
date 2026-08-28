let () =
  let table = Hashtbl.create 1 in
  let save id name = Hashtbl.replace table id name in
  save 1 "Ada";
  assert (Hashtbl.find table 1 = "Ada")
