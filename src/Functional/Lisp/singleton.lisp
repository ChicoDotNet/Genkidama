(defparameter *registry* (list :count 0))

(defun registry-instance ()
  *registry*)

(let ((first (registry-instance))
      (second (registry-instance)))
  (incf (getf first :count))
  (format t "same=~a~%" (if (eq first second) "true" "false"))
  (format t "count=~d~%" (getf second :count)))
