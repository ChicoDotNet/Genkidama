namespace Genkidama.Contracts;

/// <summary>
/// Represents a normalized paged collection result.
/// </summary>
/// <typeparam name="TItem">The item type.</typeparam>
/// <param name="Items">The returned items.</param>
/// <param name="TotalCount">The total number of items available.</param>
/// <param name="Query">The query that produced the collection.</param>
public sealed record StandardCollectionResult<TItem>(
    IReadOnlyList<TItem> Items,
    int TotalCount,
    StandardQuery Query)
{
    /// <summary>
    /// Creates a collection result from the supplied values.
    /// </summary>
    /// <param name="items">The returned items.</param>
    /// <param name="totalCount">The total number of items available.</param>
    /// <param name="query">The query that produced the collection.</param>
    /// <returns>The collection result.</returns>
    public static StandardCollectionResult<TItem> From(
        IReadOnlyList<TItem> items,
        int totalCount,
        StandardQuery query)
        => new(items, totalCount, query);
}
