iterate([],[]). iterate([H|T],[H|R]):-iterate(T,R). main:-iterate([3,2,1],[3,2,1]). :- initialization(main,main).
