(defstruct (device (:constructor %make-device (power-on mute)))
  power-on
  mute)

(defun create-device (name)
  (%make-device
   (lambda () (format nil "~A:on" name))
   (lambda () (format nil "~A:muted" name))))

(defun activate-basic (device)
  (funcall (device-power-on device)))

(defun activate-mute (device)
  (funcall (device-mute device)))

(let ((tv (create-device "TV"))
      (radio (create-device "Radio")))
  (format t "basic-tv=~A~%" (activate-basic tv))
  (format t "basic-radio=~A~%" (activate-basic radio))
  (format t "mute-tv=~A~%" (activate-mute tv))
  (format t "mute-radio=~A~%" (activate-mute radio)))
