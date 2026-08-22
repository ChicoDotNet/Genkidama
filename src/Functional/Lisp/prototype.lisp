(defstruct service-profile
  name
  features)

(defun clone-profile (profile)
  (make-service-profile
   :name (service-profile-name profile)
   :features (copy-list (service-profile-features profile))))

(defun describe-profile (profile)
  (format nil "~a: ~{~a~^,~}"
          (service-profile-name profile)
          (service-profile-features profile)))

(let* ((original (make-service-profile :name "orders" :features (list "metrics")))
       (canary (clone-profile original)))
  (setf (service-profile-name canary) "orders-canary")
  (setf (service-profile-features canary)
        (append (service-profile-features canary) (list "tracing")))
  (format t "original=~a~%" (describe-profile original))
  (format t "clone=~a~%" (describe-profile canary)))
