(defun generate-report (report-type)
  (cond ((eq report-type 'pdf) (generate-pdf-report))
        ((eq report-type 'html) (generate-html-report))))

(defun generate-pdf-report ()
  (print "Generating PDF report"))

(defun generate-html-report ()
  (print "Generating HTML report"))

;; Usage
(generate-report 'pdf)
(generate-report 'html)
