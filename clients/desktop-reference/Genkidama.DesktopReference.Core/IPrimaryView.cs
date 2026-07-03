namespace Genkidama.DesktopReference;

/// <summary>
/// Describes the primary desktop view in MVP style.
/// </summary>
public interface IPrimaryView
{
    /// <summary>Shows a loading state.</summary>
    void ShowLoading();

    /// <summary>Shows a summary value.</summary>
    /// <param name="summary">The summary.</param>
    void ShowSummary(SummaryModel summary);

    /// <summary>Shows text to the user.</summary>
    /// <param name="text">The text.</param>
    void ShowText(string text);
}
