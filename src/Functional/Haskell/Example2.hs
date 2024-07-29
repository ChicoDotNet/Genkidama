class Database db where
  connect :: db -> IO ()
  query :: db -> IO ()

data Postgres = Postgres
data MySQL = MySQL

instance Database Postgres where
  connect _ = putStrLn "Connecting to PostgreSQL"
  query _ = putStrLn "Querying PostgreSQL"

instance Database MySQL where
  connect _ = putStrLn "Connecting to MySQL"
  query _ = putStrLn "Querying MySQL"

class DatabaseFactory df where
  createDatabase :: df -> IO (IO ())

data PostgresFactory = PostgresFactory
data MySQLFactory = MySQLFactory

instance DatabaseFactory PostgresFactory where
  createDatabase _ = return (do
    let db = Postgres
    connect db
    query db)

instance DatabaseFactory MySQLFactory where
  createDatabase _ = return (do
    let db = MySQL
    connect db
    query db)

useDatabase :: (DatabaseFactory df) => df -> IO ()
useDatabase factory = do
  action <- createDatabase factory
  action

main :: IO ()
main = do
  useDatabase PostgresFactory
  useDatabase MySQLFactory
