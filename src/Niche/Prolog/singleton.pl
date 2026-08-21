:- dynamic registry_count/1.

registry_count(0).

registry_instance(registry).

registry_increment :-
    retract(registry_count(Current)),
    Next is Current + 1,
    assertz(registry_count(Next)).

run :-
    registry_instance(First),
    registry_instance(Second),
    registry_increment,
    ( First == Second -> Same = true ; Same = false ),
    registry_count(Count),
    format('same=~w~n', [Same]),
    format('count=~d~n', [Count]).
