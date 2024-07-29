# Abstract Factory
class DatabaseFactory
    def create_database
      raise NotImplementedError, 'create_database() must be implemented'
    end
  end
  
  # Concrete Factories
  class PostgresFactory < DatabaseFactory
    def create_database
      Postgres.new
    end
  end
  
  class MySQLFactory < DatabaseFactory
    def create_database
      MySQL.new
    end
  end
  
  # Products
  class Postgres
    def connect
      puts 'Connecting to PostgreSQL'
    end
  
    def query
      puts 'Querying PostgreSQL'
    end
  end
  
  class MySQL
    def connect
      puts 'Connecting to MySQL'
    end
  
    def query
      puts 'Querying MySQL'
    end
  end
  
  def use_database(factory)
    db = factory.create_database
    db.connect
    db.query
  end
  
  # Usage
  use_database(PostgresFactory.new)
  use_database(MySQLFactory.new)
  