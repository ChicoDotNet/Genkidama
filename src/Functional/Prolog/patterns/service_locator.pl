service(clock,'12:00'). locate(Name,V):-service(Name,V). main:-locate(clock,'12:00'). :- initialization(main,main).
