plugin(upper,Text,Result):-upcase_atom(Text,Result). main:-plugin(upper,plugin,'PLUGIN'). :- initialization(main,main).
