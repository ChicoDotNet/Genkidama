notify(Subs,E,Seen):-maplist(call_with(E),Subs,Seen). call_with(E,F,R):-call(F,E,R). seen(E,E). main:-notify([seen],changed,[changed]). :- initialization(main,main).
