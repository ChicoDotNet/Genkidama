(labels ((ev(n)(case(first n)(lit(second n))(var 4)(add(+ (ev(second n))(ev(third n)))))))(assert (= 7 (ev '(add (var x)(lit 3))))))
