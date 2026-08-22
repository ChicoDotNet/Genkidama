type node =
  | File of int
  | Folder of node list

let rec size = function
  | File bytes -> bytes
  | Folder children -> List.fold_left (fun total child -> total + size child) 0 children

let () =
  let readme = File 2 in
  let docs = Folder [File 3; File 5] in
  let root = Folder [readme; docs] in
  Printf.printf "leaf=%d\n" (size readme);
  Printf.printf "docs=%d\n" (size docs);
  Printf.printf "root=%d\n" (size root)
