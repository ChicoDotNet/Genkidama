trait Database {
  def connect(): Unit
  def query(): Unit
}

class Postgres extends Database {
  def connect(): Unit = println("Connecting to PostgreSQL")
  def query(): Unit = println("Querying PostgreSQL")
}

class MySQL extends Database {
  def connect(): Unit = println("Connecting to MySQL")
  def query(): Unit = println("Querying MySQL")
}

trait DatabaseFactory {
  def createDatabase(): Database
}

class PostgresFactory extends DatabaseFactory {
  def createDatabase(): Database = new Postgres
}

class MySQLFactory extends DatabaseFactory {
  def createDatabase(): Database = new MySQL
}

object Example2 {
  def useDatabase(factory: DatabaseFactory): Unit = {
    val db = factory.createDatabase()
    db.connect()
    db.query()
  }

  def main(args: Array[String]): Unit = {
    useDatabase(new PostgresFactory)
    useDatabase(new MySQLFactory)
  }
}
