device(tv, 'TV').
device(radio, 'Radio').

power_on(Device, Result) :-
    device(Device, Name),
    format(atom(Result), '~w:on', [Name]).

mute(Device, Result) :-
    device(Device, Name),
    format(atom(Result), '~w:muted', [Name]).

activate_basic(Device, Result) :- power_on(Device, Result).
activate_mute(Device, Result) :- mute(Device, Result).

run :-
    activate_basic(tv, BasicTv),
    activate_basic(radio, BasicRadio),
    activate_mute(tv, MuteTv),
    activate_mute(radio, MuteRadio),
    format('basic-tv=~w~n', [BasicTv]),
    format('basic-radio=~w~n', [BasicRadio]),
    format('mute-tv=~w~n', [MuteTv]),
    format('mute-radio=~w~n', [MuteRadio]).
