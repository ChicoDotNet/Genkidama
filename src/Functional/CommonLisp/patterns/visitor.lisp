(let ((node '(:value 5)))
  (flet ((visit (value)
           (* 2 (getf value :value))))
    (assert (= 10 (visit node)))))
