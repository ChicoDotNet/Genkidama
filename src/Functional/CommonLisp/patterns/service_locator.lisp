(let((services(list(cons 'clock(lambda()"12:00")))))(assert(string=(funcall(cdr(assoc 'clock services)))"12:00")))
