(let((handlers(list(cons 'price(lambda(x)(declare(ignore x))9)))))(assert(=9(funcall(cdr(assoc 'price handlers))"A"))))
