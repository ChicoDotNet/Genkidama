-module(lazy_initialization).
-export([main/0]).

initialize(undefined) -> {7, 1};
initialize(Value) -> {Value, 0}.

main() ->
    {Value, FirstInitializations} = initialize(undefined),
    {7, SecondInitializations} = initialize(Value),
    1 = FirstInitializations + SecondInitializations,
    ok.
