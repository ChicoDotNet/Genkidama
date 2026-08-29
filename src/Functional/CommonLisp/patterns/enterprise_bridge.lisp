(flet ((sender (message)
         (concatenate 'string "sms:" message)))
  (assert (string= (sender "ok") "sms:ok")))
