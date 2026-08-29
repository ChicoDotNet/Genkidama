(flet ((render (body)
         (format nil "<~A>" (funcall body))))
  (assert (string= (render (lambda () "sales")) "<sales>")))
