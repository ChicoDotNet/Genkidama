data Database = Database
  { connect :: IO ()
  , query :: IO ()
  }

createPostgres :: IO Database
createPostgres = pure Database
  { connect = putStrLn "PostgreSQL connect"
  , query = putStrLn "PostgreSQL query"
  }

createMySql :: IO Database
createMySql = pure Database
  { connect = putStrLn "MySQL connect"
  , query = putStrLn "MySQL query"
  }

useDatabase :: IO Database -> IO ()
useDatabase createDatabase = do
  database <- createDatabase
  connect database
  query database

main :: IO ()
main = do
  useDatabase createPostgres
  useDatabase createMySql
