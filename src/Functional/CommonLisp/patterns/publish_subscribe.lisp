(let ((received nil)
      (topic nil))
  (setf topic
        (list (lambda (value)
                (push value received))))
  (mapc (lambda (subscriber)
          (funcall subscriber "v1"))
        topic)
  (assert (equal received '("v1"))))
