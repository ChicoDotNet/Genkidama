controller(model(C),model(N)):-N is C+1. view(model(C),Text):-format(atom(Text),'count=~w',[C]). main:-controller(model(0),M),view(M,'count=1'). :- initialization(main,main).
