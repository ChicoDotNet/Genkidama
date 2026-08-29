(let ((seen nil))
  (dolist (value '(3 2 1))
    (push value seen))
  (assert (equal (reverse seen) '(3 2 1))))
