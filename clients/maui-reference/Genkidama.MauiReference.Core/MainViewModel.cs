using System.Windows.Input;

namespace Genkidama.MauiReference;

/// <summary>
/// Provides the main MAUI reference view model.
/// </summary>
public sealed class MainViewModel : ObservableObject
{
    private readonly MauiBackendClient client;
    private bool isBusy;
    private string title = "Genkidama";
    private string statusText = "Ready.";
    private int totalItems;

    /// <summary>
    /// Initializes a new instance of the <see cref="MainViewModel"/> class.
    /// </summary>
    public MainViewModel(MauiBackendClient client)
    {
        this.client = client;
        RefreshCommand = new AsyncCommand(RefreshAsync, () => !IsBusy);
    }

    /// <summary>Gets whether the view model is busy.</summary>
    public bool IsBusy
    {
        get => isBusy;
        private set => SetProperty(ref isBusy, value);
    }

    /// <summary>Gets the title.</summary>
    public string Title
    {
        get => title;
        private set => SetProperty(ref title, value);
    }

    /// <summary>Gets the status text.</summary>
    public string StatusText
    {
        get => statusText;
        private set => SetProperty(ref statusText, value);
    }

    /// <summary>Gets the total item count.</summary>
    public int TotalItems
    {
        get => totalItems;
        private set => SetProperty(ref totalItems, value);
    }

    /// <summary>Gets the refresh command.</summary>
    public ICommand RefreshCommand { get; }

    /// <summary>
    /// Refreshes the dashboard summary.
    /// </summary>
    public async Task RefreshAsync()
    {
        IsBusy = true;
        StatusText = "Loading...";
        try
        {
            await LoadSummaryAsync();
        }
        finally
        {
            IsBusy = false;
        }
    }

    private async Task LoadSummaryAsync()
    {
        var result = await client.GetSummaryAsync();
        if (result.Succeeded && result.Value is not null)
        {
            Title = result.Value.Title;
            TotalItems = result.Value.TotalItems;
            StatusText = "Loaded.";
            return;
        }

        StatusText = result.Text ?? "No data.";
    }
}
