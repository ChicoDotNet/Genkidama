(let((seen nil))(dolist(x '(3 2 1))(push x seen))(assert(equal(reverse seen)'(3 2 1))))
