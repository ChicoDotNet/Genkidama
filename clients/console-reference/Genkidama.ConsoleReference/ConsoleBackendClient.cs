using System.Net.Http.Json;

namespace Genkidama.ConsoleReference;

/// <summary>
/// Provides typed access to a Genkidama backend from console applications.
/// </summary>
public sealed class ConsoleBackendClient
{
    private readonly HttpClient httpClient;

    /// <summary>
    /// Initializes a new instance of the <see cref="ConsoleBackendClient"/> class.
    /// </summary>
    /// <param name="httpClient">The configured HTTP client.</param>
    public ConsoleBackendClient(HttpClient httpClient)
        => this.httpClient = httpClient;

    /// <summary>
    /// Reads a typed standard result from the backend.
    /// </summary>
    /// <typeparam name="TValue">The value type.</typeparam>
    /// <param name="path">The relative API path.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The typed API result.</returns>
    public async Task<ApiResult<TValue>> GetResultAsync<TValue>(
        string path,
        CancellationToken cancellationToken = default)
    {
        var response = await httpClient.GetAsync(path, cancellationToken);
        return await ReadResultAsync<TValue>(response, cancellationToken);
    }

    /// <summary>
    /// Reads a typed collection from the backend.
    /// </summary>
    /// <typeparam name="TItem">The item type.</typeparam>
    /// <param name="path">The relative API path.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The typed API collection.</returns>
    public async Task<ApiCollection<TItem>?> GetCollectionAsync<TItem>(
        string path,
        CancellationToken cancellationToken = default)
        => (await GetResultAsync<ApiCollection<TItem>>(path, cancellationToken)).Value;

    private static async Task<ApiResult<TValue>> ReadResultAsync<TValue>(
        HttpResponseMessage response,
        CancellationToken cancellationToken)
        => response.IsSuccessStatusCode
            ? await response.Content.ReadFromJsonAsync<ApiResult<TValue>>(cancellationToken) ?? Failed<TValue>("Empty response.")
            : Failed<TValue>(response.ReasonPhrase ?? "Request failed.");

    private static ApiResult<TValue> Failed<TValue>(string message)
        => new() { Succeeded = false, Problem = new StandardApiProblem("http_error", message) };
}
