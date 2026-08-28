(let ((b 100))(dolist(c (list (lambda()(incf b 50))(lambda()(decf b 20))))(funcall c))(assert (= b 130)))
