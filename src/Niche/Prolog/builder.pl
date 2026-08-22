text_builder(builder(text)).
html_builder(builder(html)).

add_title(builder(text), Title, Parts, Result) :-
    atom_concat('# ', Title, Line),
    append(Parts, [Line], Result).
add_title(builder(html), Title, Parts, Result) :-
    atomic_list_concat(['<h1>', Title, '</h1>'], Line),
    append(Parts, [Line], Result).

add_section(builder(text), Heading, Body, Parts, Result) :-
    atom_concat('## ', Heading, Line),
    append(Parts, [Line, Body], Result).
add_section(builder(html), Heading, Body, Parts, Result) :-
    atomic_list_concat(['<h2>', Heading, '</h2>'], H),
    atomic_list_concat(['<p>', Body, '</p>'], B),
    append(Parts, [H, B], Result).

build(builder(text), Parts, Result) :- atomic_list_concat(Parts, '\n', Result).
build(builder(html), Parts, Result) :- atomic_list_concat(Parts, '', Result).

build_availability_report(Builder, Result) :-
    add_title(Builder, 'Service status', [], P1),
    add_section(Builder, 'Availability', '99.95%', P1, P2),
    build(Builder, P2, Result).

run :-
    text_builder(Text),
    html_builder(Html),
    build_availability_report(Text, TextResult),
    build_availability_report(Html, HtmlResult),
    format('~w~n---~n~w~n', [TextResult, HtmlResult]).
