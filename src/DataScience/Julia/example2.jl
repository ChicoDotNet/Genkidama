abstract type Database end

struct Postgres <: Database end
struct MySQL <: Database end

function connect(db::Postgres)
    println("Connecting to PostgreSQL")
end

function query(db::Postgres)
    println("Querying PostgreSQL")
end

function connect(db::MySQL)
    println("Connecting to MySQL")
end

function query(db::MySQL)
    println("Querying MySQL")
end

abstract type DatabaseFactory end

struct PostgresFactory <: DatabaseFactory end
struct MySQLFactory <: DatabaseFactory end

function create_database(::PostgresFactory)::Database
    return Postgres()
end

function create_database(::MySQLFactory)::Database
    return MySQL()
end

function use_database(factory::DatabaseFactory)
    db = create_database(factory)
    connect(db)
    query(db)
end

# Usage
use_database(PostgresFactory())
use_database(MySQLFactory())
