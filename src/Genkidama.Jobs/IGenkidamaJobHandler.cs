using Genkidama.Contracts;

namespace Genkidama.Jobs;

/// <summary>
/// Handles one background job type.
/// </summary>
/// <typeparam name="TJob">The job type.</typeparam>
/// <typeparam name="TResponse">The response type.</typeparam>
public interface IGenkidamaJobHandler<TJob, TResponse>
    where TJob : IGenkidamaJob<TResponse>
{
    /// <summary>
    /// Handles the supplied job.
    /// </summary>
    /// <param name="job">The job.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The job result.</returns>
    Task<StandardResult<TResponse>> HandleAsync(
        TJob job,
        CancellationToken cancellationToken = default);
}
