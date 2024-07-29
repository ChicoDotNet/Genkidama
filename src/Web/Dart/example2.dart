// Abstract Product
abstract class Database {
  void connect();
  void query();
}

// Concrete Product
class Postgres implements Database {
  @override
  void connect() {
    print('Connecting to PostgreSQL');
  }

  @override
  void query() {
    print('Querying PostgreSQL');
  }
}

class MySQL implements Database {
  @override
  void connect() {
    print('Connecting to MySQL');
  }

  @override
  void query() {
    print('Querying MySQL');
  }
}

// Abstract Factory
abstract class DatabaseFactory {
  Database createDatabase();
}

// Concrete Factory
class PostgresFactory implements DatabaseFactory {
  @override
  Database createDatabase() {
    return Postgres();
  }
}

class MySQLFactory implements DatabaseFactory {
  @override
  Database createDatabase() {
    return MySQL();
  }
}

void useDatabase(DatabaseFactory factory) {
  var db = factory.createDatabase();
  db.connect();
  db.query();
}

// Usage
void main() {
  useDatabase(PostgresFactory());
  useDatabase(MySQLFactory());
}
