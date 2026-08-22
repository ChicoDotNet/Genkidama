-module(composite).
-export([main/0]).

size({file, Bytes}) -> Bytes;
size({folder, Children}) -> lists:sum([size(Child) || Child <- Children]).

main() ->
    Readme = {file, 2},
    Docs = {folder, [{file, 3}, {file, 5}]},
    Root = {folder, [Readme, Docs]},
    io:format("leaf=~p~n", [size(Readme)]),
    io:format("docs=~p~n", [size(Docs)]),
    io:format("root=~p~n", [size(Root)]).
