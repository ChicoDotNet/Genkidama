(let((received nil)(topic nil))(setf topic(list(lambda(v)(push v received))))(mapc(lambda(f)(funcall f "v1"))topic)(assert(equal received '("v1"))))
