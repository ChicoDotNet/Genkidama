stock(available). charge(paid). checkout(Result):-stock(available),charge(Result). main:-checkout(paid). :- initialization(main,main).
