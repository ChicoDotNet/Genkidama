-module(composite).
-export([main/0]).

node_size({file, Bytes}) -> Bytes;
node_size({folder, Children}) -> lists:sum([node_size(Child) || Child <- Children]).

main() ->
    Readme = {file, 2},
    Docs = {folder, [{file, 3}, {file, 5}]},
    Root = {folder, [Readme, Docs]},
    io:format("leaf=~p~n", [node_size(Readme)]),
    io:format("docs=~p~n", [node_size(Docs)]),
    io:format("root=~p~n", [node_size(Root)]).
