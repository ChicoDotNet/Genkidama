namespace Genkidama.MauiReference;

/// <summary>
/// Provides source-only MAUI app registration for the reference client.
/// </summary>
public static class MauiProgram
{
    /// <summary>
    /// Creates the MAUI application.
    /// </summary>
    public static MauiApp CreateMauiApp()
    {
        var builder = MauiApp.CreateBuilder();
        builder.UseMauiApp<App>();
        builder.Services.AddSingleton(new HttpClient { BaseAddress = new Uri("http://localhost:5000/") });
        builder.Services.AddSingleton<MauiBackendClient>();
        builder.Services.AddTransient<MainViewModel>();
        builder.Services.AddTransient<MainPage>();
        return builder.Build();
    }
}
