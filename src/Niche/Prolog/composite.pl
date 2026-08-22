node_size(file(Bytes), Bytes).
node_size(folder(Children), Total) :-
    maplist(node_size, Children, Sizes),
    sum_list(Sizes, Total).

run :-
    Readme = file(2),
    Docs = folder([file(3), file(5)]),
    Root = folder([Readme, Docs]),
    node_size(Readme, LeafSize),
    node_size(Docs, DocsSize),
    node_size(Root, RootSize),
    format('leaf=~d~n', [LeafSize]),
    format('docs=~d~n', [DocsSize]),
    format('root=~d~n', [RootSize]).
