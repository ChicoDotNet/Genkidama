using Genkidama.Contracts;

namespace Genkidama.Jobs;

/// <summary>
/// Dispatches background jobs to handlers.
/// </summary>
public interface IGenkidamaJobDispatcher
{
    /// <summary>
    /// Sends a job to its handler.
    /// </summary>
    /// <typeparam name="TJob">The job type.</typeparam>
    /// <typeparam name="TResponse">The response type.</typeparam>
    /// <param name="job">The job.</param>
    /// <param name="handler">The job handler.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The job result.</returns>
    Task<StandardResult<TResponse>> SendAsync<TJob, TResponse>(
        TJob job,
        IGenkidamaJobHandler<TJob, TResponse> handler,
        CancellationToken cancellationToken = default)
        where TJob : IGenkidamaJob<TResponse>;
}
