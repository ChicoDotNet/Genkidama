(let((state 'closed))(setf state(if(eq state 'closed)'open 'closed))(assert(eq state 'open)))
