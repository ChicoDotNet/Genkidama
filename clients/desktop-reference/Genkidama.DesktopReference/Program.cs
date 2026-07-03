using System.Windows.Forms;

namespace Genkidama.DesktopReference;

/// <summary>
/// Provides the desktop reference entry point.
/// </summary>
public static class Program
{
    /// <summary>
    /// Runs the desktop reference client.
    /// </summary>
    [STAThread]
    public static void Main()
    {
        ApplicationConfiguration.Initialize();
        using var httpClient = new HttpClient { BaseAddress = new Uri("http://localhost:5000/") };
        var backend = new DesktopBackendClient(httpClient);
        Application.Run(new PrimaryForm(view => new PrimaryPresenter(view, backend)));
    }
}
