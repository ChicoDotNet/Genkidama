-module(factory_method).
-export([main/0]).

create_postgres() ->
    #{connect => fun() -> io:format("PostgreSQL connect~n") end,
      query => fun() -> io:format("PostgreSQL query~n") end}.

create_mysql() ->
    #{connect => fun() -> io:format("MySQL connect~n") end,
      query => fun() -> io:format("MySQL query~n") end}.

use_database(CreateDatabase) ->
    Database = CreateDatabase(),
    Connect = maps:get(connect, Database),
    Query = maps:get(query, Database),
    Connect(),
    Query().

main() ->
    use_database(fun create_postgres/0),
    use_database(fun create_mysql/0).
