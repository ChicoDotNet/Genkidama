protocol FactoryMethodDatabase {
    func connect()
    func query()
}

struct FactoryMethodPostgres: FactoryMethodDatabase {
    func connect() { print("PostgreSQL connect") }
    func query() { print("PostgreSQL query") }
}

struct FactoryMethodMySql: FactoryMethodDatabase {
    func connect() { print("MySQL connect") }
    func query() { print("MySQL query") }
}

func useDatabase(createDatabase: () -> any FactoryMethodDatabase) {
    let database = createDatabase()
    database.connect()
    database.query()
}

useDatabase { FactoryMethodPostgres() }
useDatabase { FactoryMethodMySql() }
