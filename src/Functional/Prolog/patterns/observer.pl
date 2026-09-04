subscribe(Observer, Subscribers, Subscribers, false) :-
    memberchk(Observer, Subscribers), !.
subscribe(Observer, Subscribers, Updated, true) :-
    append(Subscribers, [Observer], Updated).

unsubscribe(Observer, Subscribers, Updated, true) :-
    select(Observer, Subscribers, Updated), !.
unsubscribe(_, Subscribers, Subscribers, false).

deliver(Event, audit, audit(Event)).
deliver(Event, dashboard, dashboard(Event)).

publish(Subscribers, Event, Deliveries) :-
    findall(Delivery,
            ( member(Observer, Subscribers),
              deliver(Event, Observer, Delivery)
            ),
            Deliveries).

expect(Goal, Message) :-
    ( call(Goal) -> true ; throw(error(observer_contract_failed(Message), _)) ).

main :-
    subscribe(audit, [], S1, true),
    subscribe(dashboard, S1, S2, true),

    subscribe(audit, S2, DuplicateState, false),
    expect(DuplicateState == S2, duplicate_subscription_changed_state),

    publish(S2, changed, FirstDelivery),
    expect(FirstDelivery == [audit(changed), dashboard(changed)], first_publish_mismatch),

    unsubscribe(audit, S2, S3, true),
    unsubscribe(audit, S3, MissingState, false),
    expect(MissingState == S3, repeated_unsubscribe_changed_state),

    publish(S3, resolved, SecondDelivery),
    expect(SecondDelivery == [dashboard(resolved)], second_publish_mismatch),

    writeln('OBSERVER_PROLOG_OK').

:- initialization(main, main).
