type
  DatabaseKind = enum
    postgres, mysql
  FactoryMethod = proc(): DatabaseKind

proc createPostgres(): DatabaseKind = postgres
proc createMySql(): DatabaseKind = mysql

proc useDatabase(createDatabase: FactoryMethod) =
  case createDatabase()
  of postgres:
    echo "PostgreSQL connect"
    echo "PostgreSQL query"
  of mysql:
    echo "MySQL connect"
    echo "MySQL query"

useDatabase(createPostgres)
useDatabase(createMySql)
