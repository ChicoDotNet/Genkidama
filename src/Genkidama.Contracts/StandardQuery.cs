namespace Genkidama.Contracts;

/// <summary>
/// Describes a normalized query for collection endpoints.
/// </summary>
/// <param name="PageNumber">The one-based page number.</param>
/// <param name="PageSize">The requested page size.</param>
/// <param name="SearchText">The optional search text.</param>
public sealed record StandardQuery(
    int PageNumber = 1,
    int PageSize = 50,
    string? SearchText = null)
{
    /// <summary>
    /// Gets the number of records to skip.
    /// </summary>
    public int Skip => (PageNumber - 1) * PageSize;

    /// <summary>
    /// Creates a query for the first page.
    /// </summary>
    /// <param name="pageSize">The page size.</param>
    /// <returns>The first page query.</returns>
    public static StandardQuery FirstPage(int pageSize = 50)
        => new(1, pageSize);
}
