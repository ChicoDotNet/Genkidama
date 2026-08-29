let sender text = "sms:" ^ text
let notify text = sender text

let () = assert (notify "ok" = "sms:ok")
