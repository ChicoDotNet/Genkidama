namespace Genkidama.DesktopReference;

/// <summary>
/// Coordinates the primary desktop view.
/// </summary>
public sealed class PrimaryPresenter
{
    private readonly IPrimaryView view;
    private readonly DesktopBackendClient client;

    /// <summary>
    /// Initializes a new instance of the <see cref="PrimaryPresenter"/> class.
    /// </summary>
    public PrimaryPresenter(IPrimaryView view, DesktopBackendClient client)
    {
        this.view = view;
        this.client = client;
    }

    /// <summary>
    /// Loads data into the view.
    /// </summary>
    public async Task LoadAsync(CancellationToken cancellationToken = default)
    {
        view.ShowLoading();
        var result = await client.GetSummaryAsync(cancellationToken);
        if (result.Succeeded && result.Value is not null)
        {
            view.ShowSummary(result.Value);
        }
        else
        {
            view.ShowText(result.Text ?? "No data.");
        }
    }
}
