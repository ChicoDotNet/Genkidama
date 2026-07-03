using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Dispatches queries through the Genkidama application pipeline.
/// </summary>
public sealed class GenkidamaQueryDispatcher : IGenkidamaQueryDispatcher
{
    /// <inheritdoc />
    public Task<StandardResult<TResponse>> SendAsync<TQuery, TResponse>(
        TQuery query,
        IGenkidamaQueryHandler<TQuery, TResponse> handler,
        IEnumerable<IGenkidamaPipelineBehavior<TQuery, TResponse>> behaviors,
        CancellationToken cancellationToken = default)
        where TQuery : IGenkidamaQuery<TResponse>
    {
        var pipeline = new GenkidamaPipeline<TQuery, TResponse>(behaviors);
        return pipeline.ExecuteAsync(query, handler.HandleAsync, cancellationToken);
    }
}
