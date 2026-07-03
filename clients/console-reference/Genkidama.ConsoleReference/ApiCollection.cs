namespace Genkidama.ConsoleReference;

/// <summary>
/// Represents a typed collection returned by standard collection endpoints.
/// </summary>
/// <typeparam name="TItem">The item type.</typeparam>
public sealed class ApiCollection<TItem>
{
    /// <summary>Gets or initializes the returned items.</summary>
    public IReadOnlyList<TItem> Items { get; init; } = [];

    /// <summary>Gets or initializes the total count.</summary>
    public int TotalCount { get; init; }

    /// <summary>Gets or initializes the query metadata.</summary>
    public ApiQuery Query { get; init; } = new();
}
