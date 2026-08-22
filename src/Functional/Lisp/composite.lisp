(defstruct (file-leaf (:constructor make-file-leaf (bytes)))
  bytes)

(defstruct (folder-composite (:constructor make-folder-composite (children)))
  children)

(defun node-size (node)
  (etypecase node
    (file-leaf (file-leaf-bytes node))
    (folder-composite
     (reduce #'+ (mapcar #'node-size (folder-composite-children node)) :initial-value 0))))

(let* ((readme (make-file-leaf 2))
       (docs (make-folder-composite (list (make-file-leaf 3) (make-file-leaf 5))))
       (root (make-folder-composite (list readme docs))))
  (format t "leaf=~d~%" (node-size readme))
  (format t "docs=~d~%" (node-size docs))
  (format t "root=~d~%" (node-size root)))
