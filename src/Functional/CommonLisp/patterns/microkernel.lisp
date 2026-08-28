(let((plugins(list(cons 'upper #'string-upcase))))(assert(string=(funcall(cdr(assoc 'upper plugins))"plugin")"PLUGIN")))
