class DatabaseCreator {
  createDatabase() {
    throw new Error("createDatabase must be implemented");
  }

  useDatabase() {
    const database = this.createDatabase();
    database.connect();
    database.query();
  }
}

class PostgresCreator extends DatabaseCreator {
  createDatabase() {
    return {
      connect: () => console.log("PostgreSQL connect"),
      query: () => console.log("PostgreSQL query"),
    };
  }
}

class MySqlCreator extends DatabaseCreator {
  createDatabase() {
    return {
      connect: () => console.log("MySQL connect"),
      query: () => console.log("MySQL query"),
    };
  }
}

new PostgresCreator().useDatabase();
new MySqlCreator().useDatabase();
