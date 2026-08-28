(flet((greet(clock)(concatenate 'string "hello@"(funcall clock))))(assert(string=(greet(lambda()"noon"))"hello@noon")))
