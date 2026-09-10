:- initialization(main).

strategy(regular, Amount, Price) :-
    Price is Amount.
strategy(vip, Amount, Price) :-
    Price is Amount * 80 // 100.
strategy(campaign, Amount, Price) :-
    ( Amount >= 100 -> Price is Amount * 75 // 100 ; Price is Amount ).

apply_strategy(Strategy, Amount, Price) :-
    strategy(Strategy, Amount, Price).

expect(Label, Actual, Expected) :-
    ( Actual =:= Expected -> true
    ; format(user_error, '~w expected ~w but got ~w~n', [Label, Expected, Actual]),
      halt(1)
    ).

main :-
    apply_strategy(regular, 100, Regular),
    expect(regular, Regular, 100),
    apply_strategy(vip, 100, Vip),
    expect(vip, Vip, 80),
    apply_strategy(campaign, 100, Campaign),
    expect(campaign, Campaign, 75),
    apply_strategy(campaign, 80, Below),
    expect(below_threshold, Below, 80),
    writeln('Prolog Strategy: regular=100;vip=80;campaign=75;below=80'),
    halt(0).