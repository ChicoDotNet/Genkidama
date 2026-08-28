let null_logger _=();;let service logger=logger"run";"ok";;let ()=assert(service null_logger="ok")
