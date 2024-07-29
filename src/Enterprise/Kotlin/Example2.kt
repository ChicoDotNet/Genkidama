// Abstract Product
interface Database {
    fun connect()
    fun query()
}

// Concrete Product
class Postgres : Database {
    override fun connect() {
        println("Connecting to PostgreSQL")
    }

    override fun query() {
        println("Querying PostgreSQL")
    }
}

class MySQL : Database {
    override fun connect() {
        println("Connecting to MySQL")
    }

    override fun query() {
        println("Querying MySQL")
    }
}

// Abstract Factory
interface DatabaseFactory {
    fun createDatabase(): Database
}

// Concrete Factory
class PostgresFactory : DatabaseFactory {
    override fun createDatabase(): Database {
        return Postgres()
    }
}

class MySQLFactory : DatabaseFactory {
    override fun createDatabase(): Database {
        return MySQL()
    }
}

fun useDatabase(factory: DatabaseFactory) {
    val db = factory.createDatabase()
    db.connect()
    db.query()
}

// Usage
fun main() {
    useDatabase(PostgresFactory())
    useDatabase(MySQLFactory())
}
