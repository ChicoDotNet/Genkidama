server(Request,response(Request)). client(V,R):-server(V,response(R)). main:-client(ping,ping). :- initialization(main,main).
