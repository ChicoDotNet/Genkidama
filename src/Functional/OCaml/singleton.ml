module Registry = struct
  let state = ref 0
  let instance () = state
end

let () =
  let first = Registry.instance () in
  let second = Registry.instance () in
  incr first;
  Printf.printf "same=%b\n" (first == second);
  Printf.printf "count=%d\n" !second
