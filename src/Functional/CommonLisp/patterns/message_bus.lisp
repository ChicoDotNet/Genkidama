(let ((seen nil)
      (bus nil))
  (setf bus
        (list (lambda (value)
                (push value seen))))
  (mapc (lambda (handler)
          (funcall handler 42))
        bus)
  (assert (equal seen '(42))))
