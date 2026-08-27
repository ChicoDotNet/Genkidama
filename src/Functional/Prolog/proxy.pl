:- dynamic cached/2.
:- dynamic fetches/1.

fetches(0).

real_get(Id, Document) :-
    retract(fetches(N)),
    N1 is N + 1,
    assertz(fetches(N1)),
    format(atom(Document), 'doc(~w)', [Id]).

proxy_get(Id, Document) :-
    cached(Id, Document), !.
proxy_get(Id, Document) :-
    real_get(Id, Document),
    assertz(cached(Id, Document)).

main :-
    retractall(cached(_, _)),
    retractall(fetches(_)),
    assertz(fetches(0)),
    proxy_get(42, First),
    proxy_get(42, Second),
    fetches(FetchCount),
    format('backend=1;fetches=~w;first=~w;second=~w~n', [FetchCount, First, Second]),
    halt.

:- initialization(main, main).
