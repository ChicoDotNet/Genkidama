namespace Genkidama.DesktopReference;

/// <summary>
/// Represents the summary shown by the desktop reference client.
/// </summary>
public sealed class SummaryModel
{
    /// <summary>Gets or initializes the title.</summary>
    public string Title { get; init; } = string.Empty;

    /// <summary>Gets or initializes the total number of items.</summary>
    public int TotalItems { get; init; }
}
