namespace Genkidama.ConsoleReference;

/// <summary>
/// Represents query metadata returned by standard collection endpoints.
/// </summary>
public sealed class ApiQuery
{
    /// <summary>Gets or initializes the page number.</summary>
    public int PageNumber { get; init; }

    /// <summary>Gets or initializes the page size.</summary>
    public int PageSize { get; init; }

    /// <summary>Gets or initializes the optional search text.</summary>
    public string? SearchText { get; init; }
}
