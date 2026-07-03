using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Dispatches commands through the Genkidama application pipeline.
/// </summary>
public interface IGenkidamaCommandDispatcher
{
    /// <summary>
    /// Sends a command to its handler.
    /// </summary>
    /// <typeparam name="TCommand">The command type.</typeparam>
    /// <typeparam name="TResponse">The response type.</typeparam>
    /// <param name="command">The command.</param>
    /// <param name="handler">The command handler.</param>
    /// <param name="behaviors">The pipeline behaviors.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The command result.</returns>
    Task<StandardResult<TResponse>> SendAsync<TCommand, TResponse>(
        TCommand command,
        IGenkidamaCommandHandler<TCommand, TResponse> handler,
        IEnumerable<IGenkidamaPipelineBehavior<TCommand, TResponse>> behaviors,
        CancellationToken cancellationToken = default)
        where TCommand : IGenkidamaCommand<TResponse>;
}
