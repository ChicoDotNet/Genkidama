(let((incoming '("a" "b")))(assert(equal(mapcar #'string-upcase incoming)'("A" "B"))))
