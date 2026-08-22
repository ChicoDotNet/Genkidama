private interface FactoryMethodDatabase {
    fun connect()
    fun query()
}

private class FactoryMethodPostgres : FactoryMethodDatabase {
    override fun connect() = println("PostgreSQL connect")
    override fun query() = println("PostgreSQL query")
}

private class FactoryMethodMySql : FactoryMethodDatabase {
    override fun connect() = println("MySQL connect")
    override fun query() = println("MySQL query")
}

private abstract class FactoryMethodCreator {
    protected abstract fun createDatabase(): FactoryMethodDatabase

    fun useDatabase() {
        val database = createDatabase()
        database.connect()
        database.query()
    }
}

private class FactoryMethodPostgresCreator : FactoryMethodCreator() {
    override fun createDatabase(): FactoryMethodDatabase = FactoryMethodPostgres()
}

private class FactoryMethodMySqlCreator : FactoryMethodCreator() {
    override fun createDatabase(): FactoryMethodDatabase = FactoryMethodMySql()
}

fun main() {
    FactoryMethodPostgresCreator().useDatabase()
    FactoryMethodMySqlCreator().useDatabase()
}
