present(model(Name),view(Text)):-upcase_atom(Name,Text). main:-present(model('Ada'),view('ADA')). :- initialization(main,main).
