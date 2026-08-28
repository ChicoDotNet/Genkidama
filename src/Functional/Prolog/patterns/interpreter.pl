eval(lit(N),_,N). eval(var(X),Env,V):-memberchk(X-V,Env). eval(add(A,B),E,V):-eval(A,E,X),eval(B,E,Y),V is X+Y. main:-eval(add(var(x),lit(3)),[x-4],7). :- initialization(main,main).
