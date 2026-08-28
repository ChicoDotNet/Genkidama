send(Source,Target,Msg,delivered(Target,from(Source,Msg))). main:-send(a,b,hello,delivered(b,from(a,hello))). :- initialization(main,main).
