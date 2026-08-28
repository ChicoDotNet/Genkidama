get(none,value(7),1). get(value(V),value(V),0). main:-get(none,S,C1),get(S,value(7),C2),1 is C1+C2. :- initialization(main,main).
