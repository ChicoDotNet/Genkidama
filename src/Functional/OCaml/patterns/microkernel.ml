let () =
  let plugins = [ ("upper", String.uppercase_ascii) ] in
  let upper = List.assoc "upper" plugins in
  assert (upper "plugin" = "PLUGIN")
