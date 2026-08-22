-module(singleton).
-export([main/0]).

start() ->
    Pid = spawn(fun() -> loop(0) end),
    true = register(registry, Pid),
    Pid.

instance() -> whereis(registry).

increment() ->
    registry ! increment.

count() ->
    registry ! {count, self()},
    receive
        {count, Value} -> Value
    end.

loop(Count) ->
    receive
        increment -> loop(Count + 1);
        {count, From} ->
            From ! {count, Count},
            loop(Count);
        stop -> ok
    end.

main() ->
    _ = start(),
    First = instance(),
    Second = instance(),
    increment(),
    io:format("same=~s~n", [case First =:= Second of true -> "true"; false -> "false" end]),
    io:format("count=~B~n", [count()]),
    registry ! stop.
