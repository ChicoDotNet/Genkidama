(let ((handlers
        (list
         (cons 'price
               (lambda (request)
                 (declare (ignore request))
                 9)))))
  (assert
   (= 9
      (funcall (cdr (assoc 'price handlers)) "A"))))
