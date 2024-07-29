// Abstract Product
interface Database {
    connect(): void;
    query(): void;
}

// Concrete Product
class Postgres implements Database {
    connect(): void {
        console.log("Connecting to PostgreSQL");
    }

    query(): void {
        console.log("Querying PostgreSQL");
    }
}

class MySQL implements Database {
    connect(): void {
        console.log("Connecting to MySQL");
    }

    query(): void {
        console.log("Querying MySQL");
    }
}

// Abstract Factory
interface DatabaseFactory {
    createDatabase(): Database;
}

// Concrete Factory
class PostgresFactory implements DatabaseFactory {
    createDatabase(): Database {
        return new Postgres();
    }
}

class MySQLFactory implements DatabaseFactory {
    createDatabase(): Database {
        return new MySQL();
    }
}

function useDatabase(factory: DatabaseFactory): void {
    const db = factory.createDatabase();
    db.connect();
    db.query();
}

// Usage
useDatabase(new PostgresFactory());
useDatabase(new MySQLFactory());
