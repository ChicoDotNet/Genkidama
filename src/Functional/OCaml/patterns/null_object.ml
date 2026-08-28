let null_logger _message = ()

let service logger =
  logger "run";
  "ok"

let () = assert (service null_logger = "ok")
