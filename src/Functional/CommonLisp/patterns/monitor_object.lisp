(let((counter 0))(flet((guarded()(incf counter)))(guarded)(assert(= counter 1))))
