(let ((balance 100))
  (dolist (command
           (list (lambda () (incf balance 50))
                 (lambda () (decf balance 20))))
    (funcall command))
  (assert (= balance 130)))
