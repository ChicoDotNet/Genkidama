-module(chain_of_responsibility).
-export([main/0]).

accepts(_Amount, all) -> true;
accepts(Amount, Limit) -> Amount =< Limit.

route(Amount, [{Name, Limit} | Rest], Visited) ->
    VisitedNow = Visited ++ [Name],
    case accepts(Amount, Limit) of
        true -> {VisitedNow, Name};
        false -> route(Amount, Rest, VisitedNow)
    end;
route(_Amount, [], _Visited) ->
    error(no_handler_accepted_request).

main() ->
    Amount = 250,
    Handlers = [{"faq", 50}, {"billing", 500}, {"escalation", all}],
    {Visited, Handled} = route(Amount, Handlers, []),
    io:format("visited=~s;handled=~s;result=refund(~B)~n",
              [string:join(Visited, ">"), Handled, Amount]).
