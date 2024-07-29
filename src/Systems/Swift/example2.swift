import Foundation

// Abstract Product
protocol Database {
    func connect()
    func query()
}

// Concrete Product
class Postgres: Database {
    func connect() {
        print("Connecting to PostgreSQL")
    }
    
    func query() {
        print("Querying PostgreSQL")
    }
}

class MySQL: Database {
    func connect() {
        print("Connecting to MySQL")
    }
    
    func query() {
        print("Querying MySQL")
    }
}

// Abstract Factory
protocol DatabaseFactory {
    func createDatabase() -> Database
}

// Concrete Factory
class PostgresFactory: DatabaseFactory {
    func createDatabase() -> Database {
        return Postgres()
    }
}

class MySQLFactory: DatabaseFactory {
    func createDatabase() -> Database {
        return MySQL()
    }
}

func useDatabase(factory: DatabaseFactory) {
    let db = factory.createDatabase()
    db.connect()
    db.query()
}

// Usage
useDatabase(factory: PostgresFactory())
useDatabase(factory: MySQLFactory())
