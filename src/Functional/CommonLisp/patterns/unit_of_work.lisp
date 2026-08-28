(let((pending '(1))(db nil))(setf db(copy-list pending) pending nil)(assert(equal db '(1)))(assert(null pending)))
