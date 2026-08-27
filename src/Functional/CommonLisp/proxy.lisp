(defstruct backend
  (created 0 :type integer)
  (fetches 0 :type integer))

(defun make-proxy (state)
  (let ((cache (make-hash-table))
        (subject-created nil))
    (lambda (id)
      (multiple-value-bind (value present) (gethash id cache)
        (if present
            value
            (progn
              (unless subject-created
                (incf (backend-created state))
                (setf subject-created t))
              (incf (backend-fetches state))
              (let ((document (format nil "doc(~a)" id)))
                (setf (gethash id cache) document)
                document)))))))

(let* ((state (make-backend))
       (get-document (make-proxy state))
       (first (funcall get-document 42))
       (second (funcall get-document 42)))
  (format t "backend=~d;fetches=~d;first=~a;second=~a~%"
          (backend-created state) (backend-fetches state) first second))
