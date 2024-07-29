// Abstract Product
interface Database {
    void connect();
    void query();
}

// Concrete Product
class Postgres implements Database {
    @Override
    public void connect() {
        System.out.println("Connecting to PostgreSQL");
    }

    @Override
    public void query() {
        System.out.println("Querying PostgreSQL");
    }
}

class MySQL implements Database {
    @Override
    public void connect() {
        System.out.println("Connecting to MySQL");
    }

    @Override
    public void query() {
        System.out.println("Querying MySQL");
    }
}

// Abstract Factory
interface DatabaseFactory {
    Database createDatabase();
}

// Concrete Factory
class PostgresFactory implements DatabaseFactory {
    @Override
    public Database createDatabase() {
        return new Postgres();
    }
}

class MySQLFactory implements DatabaseFactory {
    @Override
    public Database createDatabase() {
        return new MySQL();
    }
}

public class Example2 {
    public static void useDatabase(DatabaseFactory factory) {
        Database db = factory.createDatabase();
        db.connect();
        db.query();
    }

    public static void main(String[] args) {
        useDatabase(new PostgresFactory());
        useDatabase(new MySQLFactory());
    }
}
