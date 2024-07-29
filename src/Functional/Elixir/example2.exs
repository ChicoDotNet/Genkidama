defmodule DatabaseFactory do
  def connect(:postgresql), do: Postgres.connect()
  def connect(:mysql), do: MySQL.connect()
  def query(:postgresql), do: Postgres.query()
  def query(:mysql), do: MySQL.query()
end

defmodule Postgres do
  def connect() do
    IO.puts "Connecting to PostgreSQL"
  end

  def query() do
    IO.puts "Querying PostgreSQL"
  end
end

defmodule MySQL do
  def connect() do
    IO.puts "Connecting to MySQL"
  end

  def query() do
    IO.puts "Querying MySQL"
  end
end

# Usage
DatabaseFactory.connect(:postgresql)
DatabaseFactory.query(:postgresql)
DatabaseFactory.connect(:mysql)
DatabaseFactory.query(:mysql)
