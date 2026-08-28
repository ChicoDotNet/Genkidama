:- initialization(main, main).

handler(faq, Amount) :- Amount =< 50.
handler(billing, Amount) :- Amount =< 500.
handler(escalation, _).

handle([Name|_], Amount, Visited, Handled) :-
    handler(Name, Amount), !,
    append(Visited, [Name], HandledVisited),
    atomic_list_concat(HandledVisited, '>', VisitedText),
    format('visited=~w;handled=~w;result=refund(~w)~n', [VisitedText, Name, Amount]),
    Handled = Name.
handle([Name|Rest], Amount, Visited, Handled) :-
    append(Visited, [Name], NextVisited),
    handle(Rest, Amount, NextVisited, Handled).

main :-
    handle([faq, billing, escalation], 250, [], _),
    halt.
