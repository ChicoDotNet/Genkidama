using System;

public interface IDatabase
{
    void Connect();
    void Query();
}

public sealed class PostgresDatabase : IDatabase
{
    public void Connect() => Console.WriteLine("PostgreSQL connect");
    public void Query() => Console.WriteLine("PostgreSQL query");
}

public sealed class MySqlDatabase : IDatabase
{
    public void Connect() => Console.WriteLine("MySQL connect");
    public void Query() => Console.WriteLine("MySQL query");
}

public abstract class DatabaseCreator
{
    protected abstract IDatabase CreateDatabase();

    public void UseDatabase()
    {
        var database = CreateDatabase();
        database.Connect();
        database.Query();
    }
}

public sealed class PostgresCreator : DatabaseCreator
{
    protected override IDatabase CreateDatabase() => new PostgresDatabase();
}

public sealed class MySqlCreator : DatabaseCreator
{
    protected override IDatabase CreateDatabase() => new MySqlDatabase();
}

public static class FactoryMethodExample
{
    public static void Main()
    {
        new PostgresCreator().UseDatabase();
        new MySqlCreator().UseDatabase();
    }
}
