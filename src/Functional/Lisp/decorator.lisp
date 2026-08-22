(defun base-component () "alert")

(defun audit-decorator (component)
  (lambda () (format nil "audit(~a)" (funcall component))))

(defun encrypt-decorator (component)
  (lambda () (format nil "enc(~a)" (funcall component))))

(let* ((base #'base-component)
       (audited (audit-decorator base))
       (encrypted (encrypt-decorator base))
       (stacked (audit-decorator (encrypt-decorator base))))
  (format t "base=~a~%" (funcall base))
  (format t "audit=~a~%" (funcall audited))
  (format t "encrypted=~a~%" (funcall encrypted))
  (format t "stacked=~a~%" (funcall stacked)))
