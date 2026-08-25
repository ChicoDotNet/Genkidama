-module(flyweight).
-export([main/0]).

get_style(Pool, Key, Style) ->
    case maps:find(Key, Pool) of
        {ok, Existing} -> {Existing, Pool};
        error -> {Style, maps:put(Key, Style, Pool)}
    end.

main() ->
    Pool0 = #{},
    {Red1, Pool1} = get_style(Pool0, {"Inter", 12, "red"}, #{font => "Inter", size => 12, color => "red"}),
    {Red2, Pool2} = get_style(Pool1, {"Inter", 12, "red"}, #{font => "Inter", size => 12, color => "red"}),
    {Blue, Pool3} = get_style(Pool2, {"Inter", 12, "blue"}, #{font => "Inter", size => 12, color => "blue"}),
    "blue" = maps:get(color, Blue),
    Shared = Red1 =:= Red2,
    io:format("styles=~B;shared=~p;text=ABC~n", [map_size(Pool3), Shared]).
