(flet ((send-direct (source message)
         (format nil "~A:~A" source message)))
  (assert (string= (send-direct "a" "hello") "a:hello")))
