using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Represents the next step in a Genkidama application pipeline.
/// </summary>
/// <typeparam name="TResponse">The response type.</typeparam>
/// <param name="cancellationToken">The cancellation token.</param>
/// <returns>The pipeline result.</returns>
public delegate Task<StandardResult<TResponse>> GenkidamaPipelineDelegate<TResponse>(
    CancellationToken cancellationToken = default);
