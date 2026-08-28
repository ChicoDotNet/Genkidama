(let((a 1))(flet((control(d)(incf a d))(presentation()a))(control 2)(assert(=3(presentation)))))
