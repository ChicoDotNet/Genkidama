-module(decorator).
-export([main/0]).

base_component() -> fun() -> "alert" end.
audit_decorator(Component) -> fun() -> "audit(" ++ Component() ++ ")" end.
encrypt_decorator(Component) -> fun() -> "enc(" ++ Component() ++ ")" end.

main() ->
    Base = base_component(),
    Audited = audit_decorator(Base),
    Encrypted = encrypt_decorator(Base),
    Stacked = audit_decorator(encrypt_decorator(Base)),
    io:format("base=~s~n", [Base()]),
    io:format("audit=~s~n", [Audited()]),
    io:format("encrypted=~s~n", [Encrypted()]),
    io:format("stacked=~s~n", [Stacked()]).
