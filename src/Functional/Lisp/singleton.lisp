(defparameter *registry* (list :count 0))

(defun registry-instance ()
  *registry*)

(let ((first (registry-instance))
      (second (registry-instance)))
  (incf (getf first :count))
  (format t "same=~(~a~)~%" (eq first second))
  (format t "count=~d~%" (getf second :count)))
