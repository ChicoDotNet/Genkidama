type style = { font : string; size : int; color : string }

let pool : (string, style) Hashtbl.t = Hashtbl.create 8

let key font size color = Printf.sprintf "%s|%d|%s" font size color

let get_style font size color =
  let k = key font size color in
  match Hashtbl.find_opt pool k with
  | Some value -> value
  | None ->
      let value = { font; size; color } in
      Hashtbl.add pool k value;
      value

let () =
  let red1 = get_style "Inter" 12 "red" in
  let red2 = get_style "Inter" 12 "red" in
  let blue = get_style "Inter" 12 "blue" in
  assert (blue.font = "Inter" && blue.size = 12 && blue.color = "blue");
  Printf.printf "styles=%d;shared=%s;text=ABC\n"
    (Hashtbl.length pool)
    (String.lowercase_ascii (string_of_bool (red1 == red2)))
