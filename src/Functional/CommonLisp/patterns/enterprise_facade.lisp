(flet ((stock () t)
       (charge () 'paid))
  (assert (eq (if (stock) (charge) 'sold-out) 'paid)))
