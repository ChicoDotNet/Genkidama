namespace Genkidama.MauiReference;

/// <summary>
/// Represents the summary shown by the MAUI reference client.
/// </summary>
public sealed class MauiSummaryModel
{
    /// <summary>Gets or initializes the title.</summary>
    public string Title { get; init; } = string.Empty;

    /// <summary>Gets or initializes the total number of items.</summary>
    public int TotalItems { get; init; }
}
