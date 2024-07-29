module type UIFactory = sig
  val create_button : unit -> unit
  val create_checkbox : unit -> unit
end

module DarkFactory : UIFactory = struct
  let create_button () = print_endline "Dark Button"
  let create_checkbox () = print_endline "Dark Checkbox"
end

module LightFactory : UIFactory = struct
  let create_button () = print_endline "Light Button"
  let create_checkbox () = print_endline "Light Checkbox"
end

let create_ui_components (factory : (module UIFactory)) =
  let module F = (val factory : UIFactory) in
  F.create_button ();
  F.create_checkbox ()

(* Usage *)
let () =
  create_ui_components (module DarkFactory);
  create_ui_components (module LightFactory)
