base(alert).

audit(Component, Decorated) :-
    format(atom(Decorated), 'audit(~w)', [Component]).

encrypt(Component, Decorated) :-
    format(atom(Decorated), 'enc(~w)', [Component]).

run :-
    base(Base),
    audit(Base, Audited),
    encrypt(Base, Encrypted),
    encrypt(Base, Inner),
    audit(Inner, Stacked),
    format('base=~w~n', [Base]),
    format('audit=~w~n', [Audited]),
    format('encrypted=~w~n', [Encrypted]),
    format('stacked=~w~n', [Stacked]).
