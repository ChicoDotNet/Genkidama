(defun legacy-read-fahrenheit ()
  86)

(defun adapt-to-celsius (read-fahrenheit)
  (lambda ()
    (round (* (- (funcall read-fahrenheit) 32) 5) 9)))

(let ((read-celsius (adapt-to-celsius #'legacy-read-fahrenheit)))
  (format t "legacy=~DF~%" (legacy-read-fahrenheit))
  (format t "adapted=~DC~%" (funcall read-celsius)))
