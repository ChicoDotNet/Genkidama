let choose values strategy = strategy values

let () =
  let values = [ 3; 1; 2 ] in
  let minimum = List.fold_left min max_int in
  let maximum = List.fold_left max min_int in
  assert (choose values minimum = 1);
  assert (choose values maximum = 3)
