-module(facade).
-export([main/0]).

authenticate(User) -> "auth(" ++ User ++ ")".
reserve(Sku) -> "reserve(" ++ Sku ++ ")".
charge(Cents) -> "charge(" ++ integer_to_list(Cents) ++ ")".

checkout(User, Sku, Cents) ->
    authenticate(User) ++ ">" ++ reserve(Sku) ++ ">" ++ charge(Cents).

main() ->
    io:format("checkout=~s~n", [checkout("alice", "SKU-42", 499)]).
