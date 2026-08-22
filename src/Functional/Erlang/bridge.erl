-module(bridge).
-export([main/0]).

make_device(Name) ->
    #{power_on => fun() -> Name ++ ":on" end,
      mute => fun() -> Name ++ ":muted" end}.

activate_basic(Device) ->
    PowerOn = maps:get(power_on, Device),
    PowerOn().

activate_mute(Device) ->
    Mute = maps:get(mute, Device),
    Mute().

main() ->
    Tv = make_device("TV"),
    Radio = make_device("Radio"),
    io:format("basic-tv=~s~n", [activate_basic(Tv)]),
    io:format("basic-radio=~s~n", [activate_basic(Radio)]),
    io:format("mute-tv=~s~n", [activate_mute(Tv)]),
    io:format("mute-radio=~s~n", [activate_mute(Radio)]).
