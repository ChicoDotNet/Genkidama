choose(Xs,min,R):-min_list(Xs,R). choose(Xs,max,R):-max_list(Xs,R). main:-choose([3,1,2],min,1),choose([3,1,2],max,3). :- initialization(main,main).
