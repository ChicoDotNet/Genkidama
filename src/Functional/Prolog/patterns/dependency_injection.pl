clock(noon). greet(Clock,Text):-call(Clock,T),atomic_list_concat(['hello@',T],Text). main:-greet(clock,'hello@noon'). :- initialization(main,main).
