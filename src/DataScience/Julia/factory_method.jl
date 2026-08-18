struct Database
    connect::Function
    query::Function
end

create_postgres() = Database(
    () -> println("PostgreSQL connect"),
    () -> println("PostgreSQL query"),
)

create_mysql() = Database(
    () -> println("MySQL connect"),
    () -> println("MySQL query"),
)

function use_database(create_database::Function)
    database = create_database()
    database.connect()
    database.query()
end

use_database(create_postgres)
use_database(create_mysql)
