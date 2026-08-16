(defstruct ui-factory
  create-button
  create-checkbox)

(defun dark-factory ()
  (make-ui-factory
   :create-button (lambda () (format t "Dark Button~%"))
   :create-checkbox (lambda () (format t "Dark Checkbox~%"))))

(defun light-factory ()
  (make-ui-factory
   :create-button (lambda () (format t "Light Button~%"))
   :create-checkbox (lambda () (format t "Light Checkbox~%"))))

(defun render-ui (factory)
  (funcall (ui-factory-create-button factory))
  (funcall (ui-factory-create-checkbox factory)))

(render-ui (dark-factory))
(render-ui (light-factory))
