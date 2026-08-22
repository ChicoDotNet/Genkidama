-module(builder).
-export([main/0]).

text_builder() ->
    #{reset => fun() -> [] end,
      add_title => fun(Title, Parts) -> Parts ++ ["# " ++ Title] end,
      add_section => fun(Heading, Body, Parts) -> Parts ++ ["## " ++ Heading, Body] end,
      build => fun(Parts) -> string:join(Parts, "\n") end}.

html_builder() ->
    #{reset => fun() -> [] end,
      add_title => fun(Title, Parts) -> Parts ++ ["<h1>" ++ Title ++ "</h1>"] end,
      add_section => fun(Heading, Body, Parts) ->
          Parts ++ ["<h2>" ++ Heading ++ "</h2>", "<p>" ++ Body ++ "</p>"]
      end,
      build => fun(Parts) -> lists:flatten(Parts) end}.

build_availability_report(Builder) ->
    Parts0 = (maps:get(reset, Builder))(),
    Parts1 = (maps:get(add_title, Builder))("Service status", Parts0),
    Parts2 = (maps:get(add_section, Builder))("Availability", "99.95%", Parts1),
    (maps:get(build, Builder))(Parts2).

main() ->
    io:format("~s~n---~n~s~n", [build_availability_report(text_builder()), build_availability_report(html_builder())]).
