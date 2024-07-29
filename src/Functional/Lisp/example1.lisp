(defun create-button (theme)
  (cond ((eq theme 'dark) (dark-button))
        ((eq theme 'light) (light-button))))

(defun create-checkbox (theme)
  (cond ((eq theme 'dark) (dark-checkbox))
        ((eq theme 'light) (light-checkbox))))

(defun dark-button ()
  (print "Dark Button"))

(defun light-button ()
  (print "Light Button"))

(defun dark-checkbox ()
  (print "Dark Checkbox"))

(defun light-checkbox ()
  (print "Light Checkbox"))

;; Usage
(create-button 'dark)
(create-checkbox 'light)
