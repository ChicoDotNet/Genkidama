(defparameter *style-pool* (make-hash-table :test #'equal))

(defun style-key (font size color)
  (format nil "~a|~d|~a" font size color))

(defun get-style (font size color)
  (let* ((key (style-key font size color))
         (existing (gethash key *style-pool*)))
    (or existing
        (setf (gethash key *style-pool*)
              (list :font font :size size :color color)))))

(let* ((red1 (get-style "Inter" 12 "red"))
       (red2 (get-style "Inter" 12 "red"))
       (blue (get-style "Inter" 12 "blue")))
  (assert (string= (getf blue :color) "blue"))
  (format t "styles=~d;shared=~a;text=ABC~%"
          (hash-table-count *style-pool*)
          (if (eq red1 red2) "true" "false")))
