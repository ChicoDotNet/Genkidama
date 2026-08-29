(flet ((map-row (name)
         (list :name name)))
  (assert (string= (getf (map-row "Ada") :name) "Ada")))
