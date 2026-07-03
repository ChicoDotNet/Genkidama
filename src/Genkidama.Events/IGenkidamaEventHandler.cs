using Genkidama.Contracts;

namespace Genkidama.Events;

/// <summary>
/// Handles one event type.
/// </summary>
/// <typeparam name="TEvent">The event type.</typeparam>
public interface IGenkidamaEventHandler<in TEvent>
    where TEvent : IGenkidamaEvent
{
    /// <summary>
    /// Handles the supplied event.
    /// </summary>
    /// <param name="eventItem">The event item.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The handler result.</returns>
    Task<StandardResult> HandleAsync(
        TEvent eventItem,
        CancellationToken cancellationToken = default);
}
