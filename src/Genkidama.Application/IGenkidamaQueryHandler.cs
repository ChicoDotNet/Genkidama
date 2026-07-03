using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Handles one query type.
/// </summary>
/// <typeparam name="TQuery">The query type.</typeparam>
/// <typeparam name="TResponse">The response type.</typeparam>
public interface IGenkidamaQueryHandler<TQuery, TResponse>
    where TQuery : IGenkidamaQuery<TResponse>
{
    /// <summary>
    /// Handles the supplied query.
    /// </summary>
    /// <param name="query">The query.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The query result.</returns>
    Task<StandardResult<TResponse>> HandleAsync(
        TQuery query,
        CancellationToken cancellationToken = default);
}
