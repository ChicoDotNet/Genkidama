namespace Genkidama.Data;

/// <summary>
/// Describes a configured Genkidama data provider.
/// </summary>
/// <param name="Provider">The selected database provider.</param>
/// <param name="ConnectionString">The database connection string.</param>
/// <param name="Schemas">The standard schema names.</param>
public sealed record GenkidamaDataDescriptor(
    GenkidamaDatabaseProvider Provider,
    string ConnectionString,
    IReadOnlyList<string> Schemas);
