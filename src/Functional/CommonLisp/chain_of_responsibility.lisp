(defstruct handler
  name
  accepts)

(defun route-request (amount handlers &optional (visited '()))
  (when (null handlers)
    (error "No handler accepted the request"))
  (let* ((handler (first handlers))
         (visited-now (append visited (list (handler-name handler)))))
    (if (funcall (handler-accepts handler) amount)
        (values visited-now (handler-name handler))
        (route-request amount (rest handlers) visited-now))))

(let* ((amount 250)
       (handlers
         (list
           (make-handler :name "faq" :accepts (lambda (value) (<= value 50)))
           (make-handler :name "billing" :accepts (lambda (value) (<= value 500)))
           (make-handler :name "escalation" :accepts (lambda (value) (declare (ignore value)) t)))))
  (multiple-value-bind (visited handled)
      (route-request amount handlers)
    (format t "visited=~{~a~^>~};handled=~a;result=refund(~d)~%"
            visited handled amount)))
