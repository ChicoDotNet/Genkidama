(let ((state nil)
      (mailbox nil))
  (setf mailbox
        (list (lambda ()
                (push 'done state))))
  (funcall (first mailbox))
  (assert (equal state '(done))))
