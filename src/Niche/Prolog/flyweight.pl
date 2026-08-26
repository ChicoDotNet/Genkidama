:- initialization(main, main).

intern_style(Key, Pool, Pool, Id) :-
    memberchk(Key-Id, Pool), !.
intern_style(Key, Pool0, [Key-Id|Pool0], Id) :-
    length(Pool0, Count),
    Id is Count + 1.

main :-
    intern_style(style('Inter', 12, red), [], Pool1, Red1),
    intern_style(style('Inter', 12, red), Pool1, Pool2, Red2),
    intern_style(style('Inter', 12, blue), Pool2, Pool3, Blue),
    Blue =:= 2,
    length(Pool3, Styles),
    ( Red1 =:= Red2 -> Shared = true ; Shared = false ),
    format('styles=~d;shared=~w;text=ABC~n', [Styles, Shared]).
