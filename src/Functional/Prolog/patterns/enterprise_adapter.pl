legacy(C,C). adapter(Amount,Cents):-Cents is round(Amount*100). main:-adapter(12.34,C),legacy(C,1234). :- initialization(main,main).
