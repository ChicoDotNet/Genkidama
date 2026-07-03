using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Dispatches queries through the Genkidama application pipeline.
/// </summary>
public interface IGenkidamaQueryDispatcher
{
    /// <summary>
    /// Sends a query to its handler.
    /// </summary>
    /// <typeparam name="TQuery">The query type.</typeparam>
    /// <typeparam name="TResponse">The response type.</typeparam>
    /// <param name="query">The query.</param>
    /// <param name="handler">The query handler.</param>
    /// <param name="behaviors">The pipeline behaviors.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The query result.</returns>
    Task<StandardResult<TResponse>> SendAsync<TQuery, TResponse>(
        TQuery query,
        IGenkidamaQueryHandler<TQuery, TResponse> handler,
        IEnumerable<IGenkidamaPipelineBehavior<TQuery, TResponse>> behaviors,
        CancellationToken cancellationToken = default)
        where TQuery : IGenkidamaQuery<TResponse>;
}
