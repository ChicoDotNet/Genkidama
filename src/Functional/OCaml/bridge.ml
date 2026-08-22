type device = {
  power_on : unit -> string;
  mute : unit -> string;
}

let make_device name =
  {
    power_on = (fun () -> name ^ ":on");
    mute = (fun () -> name ^ ":muted");
  }

let activate_basic device = device.power_on ()
let activate_mute device = device.mute ()

let () =
  let tv = make_device "TV" in
  let radio = make_device "Radio" in
  print_endline ("basic-tv=" ^ activate_basic tv);
  print_endline ("basic-radio=" ^ activate_basic radio);
  print_endline ("mute-tv=" ^ activate_mute tv);
  print_endline ("mute-radio=" ^ activate_mute radio)
