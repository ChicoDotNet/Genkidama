acquire([X|Rest],X,Rest). release(Pool,X,[X|Pool]). main:-acquire([obj(1)],O,P),release(P,O,[O]). :- initialization(main,main).
