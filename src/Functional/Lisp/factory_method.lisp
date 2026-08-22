(defun create-postgres ()
  (list :connect (lambda () (format t "PostgreSQL connect~%"))
        :query (lambda () (format t "PostgreSQL query~%"))))

(defun create-mysql ()
  (list :connect (lambda () (format t "MySQL connect~%"))
        :query (lambda () (format t "MySQL query~%"))))

(defun use-database (create-database)
  (let ((database (funcall create-database)))
    (funcall (getf database :connect))
    (funcall (getf database :query))))

(use-database #'create-postgres)
(use-database #'create-mysql)
