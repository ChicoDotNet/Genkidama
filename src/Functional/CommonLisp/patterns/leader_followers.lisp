(let ((handled
       (mapcar (lambda (worker event)
                 (format nil "~A:~A" worker event))
               '("leader" "follower")
               '("one" "two"))))
  (assert (equal handled '("leader:one" "follower:two"))))
