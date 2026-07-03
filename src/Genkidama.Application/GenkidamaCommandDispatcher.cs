using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Dispatches commands through the Genkidama application pipeline.
/// </summary>
public sealed class GenkidamaCommandDispatcher : IGenkidamaCommandDispatcher
{
    /// <inheritdoc />
    public Task<StandardResult<TResponse>> SendAsync<TCommand, TResponse>(
        TCommand command,
        IGenkidamaCommandHandler<TCommand, TResponse> handler,
        IEnumerable<IGenkidamaPipelineBehavior<TCommand, TResponse>> behaviors,
        CancellationToken cancellationToken = default)
        where TCommand : IGenkidamaCommand<TResponse>
    {
        var pipeline = new GenkidamaPipeline<TCommand, TResponse>(behaviors);
        return pipeline.ExecuteAsync(command, handler.HandleAsync, cancellationToken);
    }
}
