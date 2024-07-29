-module(example2).
-export([connect_db/1, query_db/1]).

% Abstract Factory
connect_db(postgresql) -> connect_postgresql();
connect_db(mysql) -> connect_mysql().

query_db(postgresql) -> query_postgresql();
query_db(mysql) -> query_mysql().

% Concrete Implementations
connect_postgresql() -> io:format("Connecting to PostgreSQL~n").
query_postgresql() -> io:format("Querying PostgreSQL~n").

connect_mysql() -> io:format("Connecting to MySQL~n").
query_mysql() -> io:format("Querying MySQL~n").

% Usage
main() ->
    connect_db(postgresql),
    query_db(postgresql),
    connect_db(mysql),
    query_db(mysql).
