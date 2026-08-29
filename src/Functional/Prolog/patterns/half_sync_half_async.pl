sync([],[]). sync([H|T],[U|R]):-upcase_atom(H,U),sync(T,R). main:-sync([a,b],['A','B']). :- initialization(main,main).
