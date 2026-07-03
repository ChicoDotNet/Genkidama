namespace Genkidama.Data;

/// <summary>
/// Provides configuration values for Genkidama data access.
/// </summary>
/// <param name="Provider">The selected database provider.</param>
/// <param name="ConnectionString">The database connection string.</param>
public sealed record GenkidamaDataOptions(
    GenkidamaDatabaseProvider Provider,
    string ConnectionString)
{
    /// <summary>
    /// Creates options for SQLite.
    /// </summary>
    /// <param name="connectionString">The SQLite connection string.</param>
    /// <returns>The configured options.</returns>
    public static GenkidamaDataOptions ForSqlite(string connectionString)
        => new(GenkidamaDatabaseProvider.Sqlite, connectionString);
}
