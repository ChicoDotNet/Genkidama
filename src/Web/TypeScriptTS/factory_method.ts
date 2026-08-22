interface Database {
  connect(): void;
  query(): void;
}

class PostgresDatabase implements Database {
  connect(): void { console.log("PostgreSQL connect"); }
  query(): void { console.log("PostgreSQL query"); }
}

class MySqlDatabase implements Database {
  connect(): void { console.log("MySQL connect"); }
  query(): void { console.log("MySQL query"); }
}

abstract class DatabaseCreator {
  protected abstract createDatabase(): Database;

  useDatabase(): void {
    const database = this.createDatabase();
    database.connect();
    database.query();
  }
}

class PostgresCreator extends DatabaseCreator {
  protected createDatabase(): Database { return new PostgresDatabase(); }
}

class MySqlCreator extends DatabaseCreator {
  protected createDatabase(): Database { return new MySqlDatabase(); }
}

new PostgresCreator().useDatabase();
new MySqlCreator().useDatabase();
