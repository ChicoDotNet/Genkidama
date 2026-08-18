type
  DatabaseAction = proc()
  Database = object
    connect: DatabaseAction
    query: DatabaseAction
  FactoryMethod = proc(): Database

proc postgresConnect() = echo "PostgreSQL connect"
proc postgresQuery() = echo "PostgreSQL query"
proc mySqlConnect() = echo "MySQL connect"
proc mySqlQuery() = echo "MySQL query"

proc createPostgres(): Database =
  Database(connect: postgresConnect, query: postgresQuery)

proc createMySql(): Database =
  Database(connect: mySqlConnect, query: mySqlQuery)

proc useDatabase(createDatabase: FactoryMethod) =
  let database = createDatabase()
  database.connect()
  database.query()

useDatabase(createPostgres)
useDatabase(createMySql)
