interface FactoryMethodDatabase {
    void connect()
    void query()
}

class FactoryMethodPostgres implements FactoryMethodDatabase {
    void connect() { println 'PostgreSQL connect' }
    void query() { println 'PostgreSQL query' }
}

class FactoryMethodMySql implements FactoryMethodDatabase {
    void connect() { println 'MySQL connect' }
    void query() { println 'MySQL query' }
}

abstract class FactoryMethodCreator {
    protected abstract FactoryMethodDatabase createDatabase()

    final void useDatabase() {
        def database = createDatabase()
        database.connect()
        database.query()
    }
}

class FactoryMethodPostgresCreator extends FactoryMethodCreator {
    protected FactoryMethodDatabase createDatabase() { new FactoryMethodPostgres() }
}

class FactoryMethodMySqlCreator extends FactoryMethodCreator {
    protected FactoryMethodDatabase createDatabase() { new FactoryMethodMySql() }
}

new FactoryMethodPostgresCreator().useDatabase()
new FactoryMethodMySqlCreator().useDatabase()
