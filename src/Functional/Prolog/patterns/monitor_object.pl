guarded_increment(counter(N),counter(M)):-M is N+1. main:-guarded_increment(counter(0),counter(1)). :- initialization(main,main).
