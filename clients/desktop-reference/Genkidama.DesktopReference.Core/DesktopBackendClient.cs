using System.Net.Http.Json;

namespace Genkidama.DesktopReference;

/// <summary>
/// Provides typed backend calls for the desktop reference client.
/// </summary>
public sealed class DesktopBackendClient
{
    private readonly HttpClient httpClient;

    /// <summary>
    /// Initializes a new instance of the <see cref="DesktopBackendClient"/> class.
    /// </summary>
    /// <param name="httpClient">The configured HTTP client.</param>
    public DesktopBackendClient(HttpClient httpClient)
        => this.httpClient = httpClient;

    /// <summary>
    /// Reads the dashboard summary.
    /// </summary>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The dashboard summary result.</returns>
    public Task<ClientResult<SummaryModel>> GetSummaryAsync(CancellationToken cancellationToken = default)
        => GetAsync<SummaryModel>("dashboard/summary", cancellationToken);

    private async Task<ClientResult<TValue>> GetAsync<TValue>(
        string path,
        CancellationToken cancellationToken)
    {
        var response = await httpClient.GetAsync(path, cancellationToken);
        if (!response.IsSuccessStatusCode)
        {
            return new ClientResult<TValue> { Succeeded = false, Text = response.ReasonPhrase };
        }

        return await response.Content.ReadFromJsonAsync<ClientResult<TValue>>(cancellationToken)
            ?? new ClientResult<TValue> { Succeeded = false, Text = "Empty response." };
    }
}
