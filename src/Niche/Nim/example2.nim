type
  Database = ref object of RootObj
  proc connect(self: Database)
  proc query(self: Database)

type
  Postgres = ref object of Database
  MySQL = ref object of Database

proc connect(self: Postgres) =
  echo "Connecting to PostgreSQL"

proc query(self: Postgres) =
  echo "Querying PostgreSQL"

proc connect(self: MySQL) =
  echo "Connecting to MySQL"

proc query(self: MySQL) =
  echo "Querying MySQL"

type
  DatabaseFactory = ref object of RootObj
  proc createDatabase(self: DatabaseFactory): Database

type
  PostgresFactory = ref object of DatabaseFactory
  MySQLFactory = ref object of DatabaseFactory

proc createDatabase(self: PostgresFactory): Database =
  Postgres()

proc createDatabase(self: MySQLFactory): Database =
  MySQL()

proc useDatabase(factory: DatabaseFactory) =
  let db = factory.createDatabase()
  db.connect()
  db.query()

# Usage
let postgresFactory = PostgresFactory()
let mysqlFactory = MySQLFactory()
useDatabase(postgresFactory)
useDatabase(mysqlFactory)
