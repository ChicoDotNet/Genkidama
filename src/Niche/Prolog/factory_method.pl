create_postgres(postgres).
create_mysql(mysql).

connect(postgres) :- writeln('PostgreSQL connect').
query(postgres) :- writeln('PostgreSQL query').
connect(mysql) :- writeln('MySQL connect').
query(mysql) :- writeln('MySQL query').

use_database(CreatePredicate) :-
    Goal =.. [CreatePredicate, Database],
    call(Goal),
    connect(Database),
    query(Database).

run :-
    use_database(create_postgres),
    use_database(create_mysql).
