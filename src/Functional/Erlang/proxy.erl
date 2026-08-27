-module(proxy).
-export([main/0]).

backend_loop(Fetches) ->
    receive
        {get, From, Id} ->
            Next = Fetches + 1,
            From ! {document, Next, lists:flatten(io_lib:format("doc(~B)", [Id]))},
            backend_loop(Next);
        {fetches, From} ->
            From ! {fetches, Fetches},
            backend_loop(Fetches);
        stop -> ok
    end.

proxy_loop(Backend, Cache) ->
    receive
        {get, From, Id} ->
            case maps:find(Id, Cache) of
                {ok, Document} ->
                    From ! {document, Document},
                    proxy_loop(Backend, Cache);
                error ->
                    Backend ! {get, self(), Id},
                    receive
                        {document, _Fetches, Document} ->
                            From ! {document, Document},
                            proxy_loop(Backend, maps:put(Id, Document, Cache))
                    end
            end;
        stop ->
            Backend ! stop,
            ok
    end.

get(Proxy, Id) ->
    Proxy ! {get, self(), Id},
    receive {document, Document} -> Document end.

main() ->
    Backend = spawn(fun() -> backend_loop(0) end),
    Proxy = spawn(fun() -> proxy_loop(Backend, #{}) end),
    First = get(Proxy, 42),
    Second = get(Proxy, 42),
    Backend ! {fetches, self()},
    Fetches = receive {fetches, Count} -> Count end,
    io:format("backend=1;fetches=~B;first=~s;second=~s~n", [Fetches, First, Second]),
    Proxy ! stop.
