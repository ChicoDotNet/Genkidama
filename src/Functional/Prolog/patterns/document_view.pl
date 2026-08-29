view_a(document(T),T). view_b(document(T),U):-upcase_atom(T,U). main:-view_a(document('One'),'One'),view_b(document('One'),'ONE'). :- initialization(main,main).
