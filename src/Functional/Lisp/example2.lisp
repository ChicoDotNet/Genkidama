(defun connect-db (db-type)
  (cond ((eq db-type 'postgresql) (connect-postgresql))
        ((eq db-type 'mysql) (connect-mysql))))

(defun query-db (db-type)
  (cond ((eq db-type 'postgresql) (query-postgresql))
        ((eq db-type 'mysql) (query-mysql))))

(defun connect-postgresql ()
  (print "Connecting to PostgreSQL"))

(defun query-postgresql ()
  (print "Querying PostgreSQL"))

(defun connect-mysql ()
  (print "Connecting to MySQL"))

(defun query-mysql ()
  (print "Querying MySQL"))

;; Usage
(connect-db 'postgresql)
(query-db 'postgresql)
(connect-db 'mysql)
(query-db 'mysql)
