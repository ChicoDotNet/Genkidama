(flet((sender(x)(concatenate 'string "sms:" x)))(assert(string=(sender "ok")"sms:ok")))
