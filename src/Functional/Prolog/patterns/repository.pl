stored(1,user('Ada')). get(Id,U):-stored(Id,U). main:-get(1,user('Ada')). :- initialization(main,main).
