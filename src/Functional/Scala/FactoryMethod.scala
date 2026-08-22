trait FactoryMethodDatabase {
  def connect(): Unit
  def query(): Unit
}

final class FactoryMethodPostgres extends FactoryMethodDatabase {
  override def connect(): Unit = println("PostgreSQL connect")
  override def query(): Unit = println("PostgreSQL query")
}

final class FactoryMethodMySql extends FactoryMethodDatabase {
  override def connect(): Unit = println("MySQL connect")
  override def query(): Unit = println("MySQL query")
}

abstract class FactoryMethodCreator {
  protected def createDatabase(): FactoryMethodDatabase

  final def useDatabase(): Unit = {
    val database = createDatabase()
    database.connect()
    database.query()
  }
}

final class FactoryMethodPostgresCreator extends FactoryMethodCreator {
  override protected def createDatabase(): FactoryMethodDatabase = new FactoryMethodPostgres
}

final class FactoryMethodMySqlCreator extends FactoryMethodCreator {
  override protected def createDatabase(): FactoryMethodDatabase = new FactoryMethodMySql
}

object FactoryMethod {
  def main(args: Array[String]): Unit = {
    new FactoryMethodPostgresCreator().useDatabase()
    new FactoryMethodMySqlCreator().useDatabase()
  }
}
