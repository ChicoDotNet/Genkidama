render(Body,Result):-atomic_list_concat(['<',Body,'>'],Result). main:-render(sales,'<sales>'). :- initialization(main,main).
