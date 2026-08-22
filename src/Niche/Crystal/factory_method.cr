abstract class Database
  abstract def connect : Nil
  abstract def query : Nil
end

class PostgresDatabase < Database
  def connect : Nil
    puts "PostgreSQL connect"
  end

  def query : Nil
    puts "PostgreSQL query"
  end
end

class MySqlDatabase < Database
  def connect : Nil
    puts "MySQL connect"
  end

  def query : Nil
    puts "MySQL query"
  end
end

abstract class DatabaseCreator
  abstract def create_database : Database

  def use_database : Nil
    database = create_database
    database.connect
    database.query
  end
end

class PostgresCreator < DatabaseCreator
  def create_database : Database
    PostgresDatabase.new
  end
end

class MySqlCreator < DatabaseCreator
  def create_database : Database
    MySqlDatabase.new
  end
end

PostgresCreator.new.use_database
MySqlCreator.new.use_database
