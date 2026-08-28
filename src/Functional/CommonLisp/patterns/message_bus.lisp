(let((seen nil)(bus nil))(setf bus(list(lambda(v)(push v seen))))(mapc(lambda(f)(funcall f 42))bus)(assert(equal seen '(42))))
