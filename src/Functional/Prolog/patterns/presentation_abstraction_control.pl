control(state(V),D,state(N)):-N is V+D. presentation(state(V),V). main:-control(state(1),2,S),presentation(S,3). :- initialization(main,main).
