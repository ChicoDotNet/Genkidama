(let((seen nil)(subs nil))(setf subs(list(lambda(e)(push e seen))))(mapc(lambda(f)(funcall f 'changed))subs)(assert(equal seen '(changed))))
