(let ((seen nil)
      (subscribers nil))
  (setf subscribers
        (list (lambda (event)
                (push event seen))))
  (mapc (lambda (subscriber)
          (funcall subscriber 'changed))
        subscribers)
  (assert (equal seen '(changed))))
