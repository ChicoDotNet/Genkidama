view_model(model(F,L),D):-atomic_list_concat([F,' ',L],D). main:-view_model(model('Ada','Lovelace'),'Ada Lovelace'). :- initialization(main,main).
