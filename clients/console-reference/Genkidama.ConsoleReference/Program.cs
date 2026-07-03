namespace Genkidama.ConsoleReference;

/// <summary>
/// Provides the console reference entry point.
/// </summary>
public static class Program
{
    /// <summary>
    /// Runs the console reference client.
    /// </summary>
    /// <param name="args">Command line arguments.</param>
    /// <returns>The process exit code.</returns>
    public static async Task<int> Main(string[] args)
    {
        using var httpClient = CreateHttpClient(args);
        var client = new ConsoleBackendClient(httpClient);
        var result = await client.GetResultAsync<DashboardSummary>("dashboard/summary");
        WriteResult(result);
        return result.Succeeded ? 0 : 1;
    }

    private static HttpClient CreateHttpClient(string[] args)
        => new() { BaseAddress = ConsoleBackendOptions.From(BaseUrl(args)).BaseAddress };

    private static string BaseUrl(string[] args)
        => args.Length > 0 ? args[0] : "http://localhost:5000/";

    private static void WriteResult(ApiResult<DashboardSummary> result)
    {
        var text = result.Succeeded ? result.Value?.Title ?? "No title." : result.Problem?.Message;
        Console.WriteLine(text ?? "Request failed.");
    }

    private sealed record DashboardSummary(string Title, int TotalItems);
}
