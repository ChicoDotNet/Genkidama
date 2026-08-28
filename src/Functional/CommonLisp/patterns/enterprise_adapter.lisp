(flet ((legacy (cents) cents)
       (adapter (amount) (round (* amount 100))))
  (assert (= 1234 (legacy (adapter 12.34)))))
