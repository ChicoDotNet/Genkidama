transition(locked, coin, unlocked).
transition(locked, push, locked).
transition(unlocked, coin, unlocked).
transition(unlocked, push, locked).

main :-
    State0 = locked,
    transition(State0, push, State1),
    State1 = locked,
    transition(State1, coin, State2),
    State2 = unlocked,
    transition(State2, coin, State3),
    State3 = unlocked,
    transition(State3, push, State4),
    State4 = locked,
    \+ transition(unknown, coin, _),
    writeln('prolog-state: passed').

:- initialization(main, main).
