remote(7,user('Ada')). proxy(Id,Name):-remote(Id,user(Name)). main:-proxy(7,'Ada'). :- initialization(main,main).
