(let ((calls 0)
      (value nil))
  (flet ((get ()
           (or value
               (setf value
                     (progn
                       (incf calls)
                       7)))))
    (get)
    (get)
    (assert (= calls 1))))
