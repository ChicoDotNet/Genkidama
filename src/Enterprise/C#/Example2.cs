using System;

// Abstract Product
public interface Database {
    void Connect();
    void Query();
}

// Concrete Product
public class Postgres : Database {
    public void Connect() {
        Console.WriteLine("Connecting to PostgreSQL");
    }
    public void Query() {
        Console.WriteLine("Querying PostgreSQL");
    }
}

public class MySQL : Database {
    public void Connect() {
        Console.WriteLine("Connecting to MySQL");
    }
    public void Query() {
        Console.WriteLine("Querying MySQL");
    }
}

// Abstract Factory
public interface DatabaseFactory {
    Database CreateDatabase();
}

// Concrete Factory
public class PostgresFactory : DatabaseFactory {
    public Database CreateDatabase() {
        return new Postgres();
    }
}

public class MySQLFactory : DatabaseFactory {
    public Database CreateDatabase() {
        return new MySQL();
    }
}

public class Example2 {
    public static void UseDatabase(DatabaseFactory factory) {
        var db = factory.CreateDatabase();
        db.Connect();
        db.Query();
    }

    public static void Main(string[] args) {
        UseDatabase(new PostgresFactory());
        UseDatabase(new MySQLFactory());
    }
}
