(let ((calls 0)
      (value nil))
  (flet ((resolve-value ()
           (or value
               (setf value
                     (progn
                       (incf calls)
                       7)))))
    (resolve-value)
    (resolve-value)
    (assert (= calls 1))))
