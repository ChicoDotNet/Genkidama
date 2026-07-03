using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Adds behavior before or after a request handler.
/// </summary>
/// <typeparam name="TRequest">The request type.</typeparam>
/// <typeparam name="TResponse">The response type.</typeparam>
public interface IGenkidamaPipelineBehavior<TRequest, TResponse>
{
    /// <summary>
    /// Handles one pipeline step.
    /// </summary>
    /// <param name="request">The request.</param>
    /// <param name="next">The next pipeline step.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The pipeline result.</returns>
    Task<StandardResult<TResponse>> HandleAsync(
        TRequest request,
        GenkidamaPipelineDelegate<TResponse> next,
        CancellationToken cancellationToken = default);
}
