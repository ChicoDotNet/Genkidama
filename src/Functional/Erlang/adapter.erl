-module(adapter).
-export([main/0]).

legacy_read_fahrenheit() -> 86.

adapt_to_celsius(ReadFahrenheit) ->
    fun() -> ((ReadFahrenheit() - 32) * 5) div 9 end.

main() ->
    ReadCelsius = adapt_to_celsius(fun legacy_read_fahrenheit/0),
    io:format("legacy=~BF~n", [legacy_read_fahrenheit()]),
    io:format("adapted=~BC~n", [ReadCelsius()]).
