type report_builder = {
  reset : unit -> unit;
  add_title : string -> unit;
  add_section : string -> string -> unit;
  build : unit -> string;
}

let make_builder html =
  let parts = ref [] in
  let reset () = parts := [] in
  let add value = parts := !parts @ [value] in
  let add_title title = if html then add ("<h1>" ^ title ^ "</h1>") else add ("# " ^ title) in
  let add_section heading body =
    if html then (add ("<h2>" ^ heading ^ "</h2>"); add ("<p>" ^ body ^ "</p>"))
    else (add ("## " ^ heading); add body)
  in
  let build () = String.concat (if html then "" else "\n") !parts in
  { reset; add_title; add_section; build }

let build_availability_report builder =
  builder.reset ();
  builder.add_title "Service status";
  builder.add_section "Availability" "99.95%";
  builder.build ()

let () =
  print_endline (build_availability_report (make_builder false));
  print_endline "---";
  print_endline (build_availability_report (make_builder true))
