create_postgres <- function() {
  list(
    connect = function() cat("PostgreSQL connect\n"),
    query = function() cat("PostgreSQL query\n")
  )
}

create_mysql <- function() {
  list(
    connect = function() cat("MySQL connect\n"),
    query = function() cat("MySQL query\n")
  )
}

use_database <- function(create_database) {
  database <- create_database()
  database$connect()
  database$query()
}

use_database(create_postgres)
use_database(create_mysql)
