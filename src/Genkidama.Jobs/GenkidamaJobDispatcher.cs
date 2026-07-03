using Genkidama.Contracts;

namespace Genkidama.Jobs;

/// <summary>
/// Dispatches background jobs to handlers.
/// </summary>
public sealed class GenkidamaJobDispatcher : IGenkidamaJobDispatcher
{
    /// <inheritdoc />
    public Task<StandardResult<TResponse>> SendAsync<TJob, TResponse>(
        TJob job,
        IGenkidamaJobHandler<TJob, TResponse> handler,
        CancellationToken cancellationToken = default)
        where TJob : IGenkidamaJob<TResponse>
        => handler.HandleAsync(job, cancellationToken);
}
