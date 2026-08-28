(flet ((choose (values strategy)
         (apply strategy values)))
  (assert (= 1 (choose '(3 1 2) #'min)))
  (assert (= 3 (choose '(3 1 2) #'max))))
