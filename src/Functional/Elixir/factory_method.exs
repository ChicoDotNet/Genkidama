defmodule FactoryMethod do
  def create_postgres do
    %{
      connect: fn -> IO.puts("PostgreSQL connect") end,
      query: fn -> IO.puts("PostgreSQL query") end
    }
  end

  def create_mysql do
    %{
      connect: fn -> IO.puts("MySQL connect") end,
      query: fn -> IO.puts("MySQL query") end
    }
  end

  def use_database(create_database) do
    database = create_database.()
    database.connect.()
    database.query.()
  end
end

FactoryMethod.use_database(&FactoryMethod.create_postgres/0)
FactoryMethod.use_database(&FactoryMethod.create_mysql/0)
