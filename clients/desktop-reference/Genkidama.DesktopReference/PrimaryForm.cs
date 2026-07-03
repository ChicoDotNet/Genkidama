using System.Windows.Forms;

namespace Genkidama.DesktopReference;

/// <summary>
/// Provides the primary desktop form.
/// </summary>
public sealed class PrimaryForm : Form, IPrimaryView
{
    private readonly Label titleLabel = new() { Dock = DockStyle.Top, Height = 40, Text = "Genkidama" };
    private readonly Label totalLabel = new() { Dock = DockStyle.Top, Height = 32 };
    private readonly Button loadButton = new() { Dock = DockStyle.Top, Height = 32, Text = "Load" };
    private readonly PrimaryPresenter presenter;

    /// <summary>
    /// Initializes a new instance of the <see cref="PrimaryForm"/> class.
    /// </summary>
    /// <param name="presenterFactory">The presenter factory.</param>
    public PrimaryForm(Func<IPrimaryView, PrimaryPresenter> presenterFactory)
    {
        Text = "Genkidama Desktop Reference";
        Controls.Add(totalLabel);
        Controls.Add(loadButton);
        Controls.Add(titleLabel);
        presenter = presenterFactory(this);
        loadButton.Click += async (_, _) => await presenter.LoadAsync();
    }

    /// <inheritdoc />
    public void ShowLoading()
        => totalLabel.Text = "Loading...";

    /// <inheritdoc />
    public void ShowSummary(SummaryModel summary)
    {
        titleLabel.Text = summary.Title;
        totalLabel.Text = $"Total items: {summary.TotalItems}";
    }

    /// <inheritdoc />
    public void ShowText(string text)
        => totalLabel.Text = text;
}
