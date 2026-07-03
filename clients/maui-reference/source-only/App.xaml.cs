namespace Genkidama.MauiReference;

/// <summary>
/// Provides the source-only MAUI application shell.
/// </summary>
public partial class App : Application
{
    /// <summary>
    /// Initializes a new instance of the <see cref="App"/> class.
    /// </summary>
    public App(MainPage mainPage)
    {
        InitializeComponent();
        MainPage = mainPage;
    }
}
