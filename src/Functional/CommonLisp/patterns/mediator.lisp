(let ((colleagues (make-hash-table :test #'equal)))
  (labels ((register (name receiver)
             (setf (gethash name colleagues) receiver))
           (send (sender recipient message)
             (let ((receiver (gethash recipient colleagues)))
               (unless receiver
                 (error "Unknown colleague: ~A" recipient))
               (funcall receiver sender message))))
    (register "payment"
              (lambda (sender message)
                (format nil "payment received ~A from ~A" message sender)))
    (register "inventory"
              (lambda (sender message)
                (format nil "inventory received ~A from ~A" message sender)))

    (assert (string= (send "payment" "inventory" "paid")
                     "inventory received paid from payment"))
    (assert (string= (send "inventory" "payment" "reserved")
                     "payment received reserved from inventory"))

    (assert
     (handler-case
         (progn
           (send "payment" "shipping" "paid")
           nil)
       (error () t)))))
