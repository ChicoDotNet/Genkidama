using System.Net.Http.Json;

namespace Genkidama.MauiReference;

/// <summary>
/// Provides typed backend calls for the MAUI reference client.
/// </summary>
public sealed class MauiBackendClient
{
    private readonly HttpClient httpClient;

    /// <summary>
    /// Initializes a new instance of the <see cref="MauiBackendClient"/> class.
    /// </summary>
    public MauiBackendClient(HttpClient httpClient)
        => this.httpClient = httpClient;

    /// <summary>
    /// Reads the dashboard summary.
    /// </summary>
    public Task<MauiClientResult<MauiSummaryModel>> GetSummaryAsync(
        CancellationToken cancellationToken = default)
        => GetAsync<MauiSummaryModel>("dashboard/summary", cancellationToken);

    private async Task<MauiClientResult<TValue>> GetAsync<TValue>(
        string path,
        CancellationToken cancellationToken)
    {
        var response = await httpClient.GetAsync(path, cancellationToken);
        if (!response.IsSuccessStatusCode)
        {
            return new MauiClientResult<TValue> { Succeeded = false, Text = response.ReasonPhrase };
        }

        return await response.Content.ReadFromJsonAsync<MauiClientResult<TValue>>(cancellationToken)
            ?? new MauiClientResult<TValue> { Succeeded = false, Text = "Empty response." };
    }
}
