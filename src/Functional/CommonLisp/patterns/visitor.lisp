(let((node '(:value 5)))(flet((visit(n)(* 2(getf n :value))))(assert(=10(visit node)))))
