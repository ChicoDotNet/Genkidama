(flet ((server (request) request)
       (client (value) value))
  (assert (string= (server (client "ping")) "ping")))
