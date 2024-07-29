interface Database {
    void connect()
    void query()
}

class Postgres implements Database {
    void connect() {
        println "Connecting to PostgreSQL"
    }
    void query() {
        println "Querying PostgreSQL"
    }
}

class MySQL implements Database {
    void connect() {
        println "Connecting to MySQL"
    }
    void query() {
        println "Querying MySQL"
    }
}

interface DatabaseFactory {
    Database createDatabase()
}

class PostgresFactory implements DatabaseFactory {
    Database createDatabase() {
        return new Postgres()
    }
}

class MySQLFactory implements DatabaseFactory {
    Database createDatabase() {
        return new MySQL()
    }
}

def useDatabase(DatabaseFactory factory) {
    def db = factory.createDatabase()
    db.connect()
    db.query()
}

// Usage
useDatabase(new PostgresFactory())
useDatabase(new MySQLFactory())
