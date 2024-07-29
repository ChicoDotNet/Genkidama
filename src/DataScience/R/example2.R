# Abstract Factory
create_database <- function(db_type) {
  if (db_type == "postgresql") {
    return(list(connect = connect_postgresql, query = query_postgresql))
  } else if (db_type == "mysql") {
    return(list(connect = connect_mysql, query = query_mysql))
  }
}

# Concrete Implementations
connect_postgresql <- function() {
  cat("Connecting to PostgreSQL\n")
}

query_postgresql <- function() {
  cat("Querying PostgreSQL\n")
}

connect_mysql <- function() {
  cat("Connecting to MySQL\n")
}

query_mysql <- function() {
  cat("Querying MySQL\n")
}

# Usage
db <- create_database("postgresql")
db$connect()
db$query()

db <- create_database("mysql")
db$connect()
db$query()
