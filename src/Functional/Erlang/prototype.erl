-module(prototype).
-export([main/0]).

clone_profile(Profile) ->
    Profile#{features := lists:map(fun(X) -> X end, maps:get(features, Profile))}.

describe(Profile) ->
    Name = maps:get(name, Profile),
    Features = string:join(maps:get(features, Profile), ","),
    Name ++ ": " ++ Features.

main() ->
    Original = #{name => "orders", features => ["metrics"]},
    BaseClone = clone_profile(Original),
    Canary = BaseClone#{name := "orders-canary", features := maps:get(features, BaseClone) ++ ["tracing"]},
    io:format("original=~s~n", [describe(Original)]),
    io:format("clone=~s~n", [describe(Canary)]).
