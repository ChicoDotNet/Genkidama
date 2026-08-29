command(100,deposit(50),150). command(X,withdraw(20),Y):-Y is X-20. main:-command(100,deposit(50),A),command(A,withdraw(20),130). :- initialization(main,main).
