abstract interface class Database {
  void connect();
  void query();
}

final class PostgresDatabase implements Database {
  @override
  void connect() => print('PostgreSQL connect');

  @override
  void query() => print('PostgreSQL query');
}

final class MySqlDatabase implements Database {
  @override
  void connect() => print('MySQL connect');

  @override
  void query() => print('MySQL query');
}

abstract class DatabaseCreator {
  Database createDatabase();

  void useDatabase() {
    final database = createDatabase();
    database.connect();
    database.query();
  }
}

final class PostgresCreator extends DatabaseCreator {
  @override
  Database createDatabase() => PostgresDatabase();
}

final class MySqlCreator extends DatabaseCreator {
  @override
  Database createDatabase() => MySqlDatabase();
}

void main() {
  PostgresCreator().useDatabase();
  MySqlCreator().useDatabase();
}
