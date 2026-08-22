clone_profile(Profile, Clone) :-
    copy_term(Profile, Clone).

describe(profile(Name, Features), Text) :-
    atomic_list_concat(Features, ',', Joined),
    atomic_list_concat([Name, ': ', Joined], Text).

run :-
    Original = profile(orders, [metrics]),
    clone_profile(Original, profile(_, BaseFeatures)),
    append(BaseFeatures, [tracing], CanaryFeatures),
    Canary = profile('orders-canary', CanaryFeatures),
    describe(Original, OriginalText),
    describe(Canary, CanaryText),
    format('original=~w~n', [OriginalText]),
    format('clone=~w~n', [CanaryText]).
