% Abstract Factory
create_database(postgresql, postgres).
create_database(mysql, mysql).

% Concrete Products
connect(postgres) :- write('Connecting to PostgreSQL'), nl.
query(postgres) :- write('Querying PostgreSQL'), nl.

connect(mysql) :- write('Connecting to MySQL'), nl.
query(mysql) :- write('Querying MySQL'), nl.

% Usage
run :-
    create_database(postgresql, PostgresDB),
    connect(PostgresDB),
    query(PostgresDB),
    create_database(mysql, MySQLDB),
    connect(MySQLDB),
    query(MySQLDB).
