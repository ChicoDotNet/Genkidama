class DatabaseCreator
  def create_database
    raise NotImplementedError
  end

  def use_database
    database = create_database
    database.connect
    database.query
  end
end

class PostgresCreator < DatabaseCreator
  def create_database
    Object.new.tap do |database|
      database.define_singleton_method(:connect) { puts "PostgreSQL connect" }
      database.define_singleton_method(:query) { puts "PostgreSQL query" }
    end
  end
end

class MySqlCreator < DatabaseCreator
  def create_database
    Object.new.tap do |database|
      database.define_singleton_method(:connect) { puts "MySQL connect" }
      database.define_singleton_method(:query) { puts "MySQL query" }
    end
  end
end

PostgresCreator.new.use_database
MySqlCreator.new.use_database
