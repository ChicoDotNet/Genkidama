subscriber(paid,V,seen(V)). publish(Topic,V,R):-subscriber(Topic,V,R). main:-publish(paid,42,seen(42)). :- initialization(main,main).
