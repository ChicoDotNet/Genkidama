(defstruct report-builder
  (html-p nil)
  (parts '()))

(defun reset-builder (builder)
  (setf (report-builder-parts builder) '()))

(defun append-part (builder part)
  (setf (report-builder-parts builder)
        (append (report-builder-parts builder) (list part))))

(defun add-title (builder title)
  (append-part builder
               (if (report-builder-html-p builder)
                   (format nil "<h1>~a</h1>" title)
                   (format nil "# ~a" title))))

(defun add-section (builder heading body)
  (if (report-builder-html-p builder)
      (progn
        (append-part builder (format nil "<h2>~a</h2>" heading))
        (append-part builder (format nil "<p>~a</p>" body)))
      (progn
        (append-part builder (format nil "## ~a" heading))
        (append-part builder body))))

(defun build (builder)
  (format nil (if (report-builder-html-p builder) "~{~a~}" "~{~a~^~%~}")
          (report-builder-parts builder)))

(defun build-availability-report (builder)
  (reset-builder builder)
  (add-title builder "Service status")
  (add-section builder "Availability" "99.95%")
  (build builder))

(format t "~a~%---~%~a~%"
        (build-availability-report (make-report-builder))
        (build-availability-report (make-report-builder :html-p t)))
