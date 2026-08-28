(let((events nil))(flet((mediate(s m)(push(format nil "~A:~A" s m)events)))(mediate 'checkout 'paid))(assert(string= (first events)"CHECKOUT:PAID")))
