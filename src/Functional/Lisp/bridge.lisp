(defstruct device power-on mute)

(defun make-device (name)
  (make-device
   :power-on (lambda () (format nil "~A:on" name))
   :mute (lambda () (format nil "~A:muted" name))))

(defun activate-basic (device)
  (funcall (device-power-on device)))

(defun activate-mute (device)
  (funcall (device-mute device)))

(let ((tv (make-device "TV"))
      (radio (make-device "Radio")))
  (format t "basic-tv=~A~%" (activate-basic tv))
  (format t "basic-radio=~A~%" (activate-basic radio))
  (format t "mute-tv=~A~%" (activate-mute tv))
  (format t "mute-radio=~A~%" (activate-mute radio)))
