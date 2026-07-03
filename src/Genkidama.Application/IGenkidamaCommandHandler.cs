using Genkidama.Contracts;

namespace Genkidama.Application;

/// <summary>
/// Handles one command type.
/// </summary>
/// <typeparam name="TCommand">The command type.</typeparam>
/// <typeparam name="TResponse">The response type.</typeparam>
public interface IGenkidamaCommandHandler<TCommand, TResponse>
    where TCommand : IGenkidamaCommand<TResponse>
{
    /// <summary>
    /// Handles the supplied command.
    /// </summary>
    /// <param name="command">The command.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The command result.</returns>
    Task<StandardResult<TResponse>> HandleAsync(
        TCommand command,
        CancellationToken cancellationToken = default);
}
