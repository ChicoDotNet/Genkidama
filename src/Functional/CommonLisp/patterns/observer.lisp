(let ((subscribers nil)
      (audit-events nil)
      (dashboard-events nil))
  (labels ((subscribe (name callback)
             (if (assoc name subscribers :test #'string=)
                 nil
                 (progn
                   (push (cons name callback) subscribers)
                   t)))
           (unsubscribe (name)
             (if (assoc name subscribers :test #'string=)
                 (progn
                   (setf subscribers
                         (delete name subscribers :key #'car :test #'string=))
                   t)
                 nil))
           (publish (event)
             (dolist (subscription (reverse subscribers))
               (funcall (cdr subscription) event))))
    (assert (subscribe "audit"
                       (lambda (event)
                         (push event audit-events))))
    (assert (subscribe "dashboard"
                       (lambda (event)
                         (push event dashboard-events))))
    (assert (not (subscribe "audit"
                            (lambda (event)
                              (declare (ignore event))))))

    (publish 'created)
    (assert (equal audit-events '(created)))
    (assert (equal dashboard-events '(created)))

    (assert (unsubscribe "dashboard"))
    (assert (not (unsubscribe "dashboard")))

    (publish 'updated)
    (assert (equal audit-events '(updated created)))
    (assert (equal dashboard-events '(created)))

    (format t "OBSERVER_COMMON_LISP_OK~%")))
