# Abstract Product
abstract class Database
  abstract def connect
  abstract def query
end

# Concrete Product
class Postgres < Database
  def connect
    puts "Connecting to PostgreSQL"
  end

  def query
    puts "Querying PostgreSQL"
  end
end

class MySQL < Database
  def connect
    puts "Connecting to MySQL"
  end

  def query
    puts "Querying MySQL"
  end
end

# Abstract Factory
abstract class DatabaseFactory
  abstract def create_database : Database
end

# Concrete Factory
class PostgresFactory < DatabaseFactory
  def create_database : Database
    Postgres.new
  end
end

class MySQLFactory < DatabaseFactory
  def create_database : Database
    MySQL.new
  end
end

def use_database(factory : DatabaseFactory)
  db = factory.create_database
  db.connect
  db.query
end

# Usage
use_database PostgresFactory.new
use_database MySQLFactory.new
