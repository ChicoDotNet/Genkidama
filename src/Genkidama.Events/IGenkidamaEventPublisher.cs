using Genkidama.Contracts;

namespace Genkidama.Events;

/// <summary>
/// Publishes events to their handlers.
/// </summary>
public interface IGenkidamaEventPublisher
{
    /// <summary>
    /// Publishes one event to the supplied handlers.
    /// </summary>
    /// <typeparam name="TEvent">The event type.</typeparam>
    /// <param name="eventItem">The event item.</param>
    /// <param name="handlers">The event handlers.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The publish result.</returns>
    Task<StandardResult> PublishAsync<TEvent>(
        TEvent eventItem,
        IEnumerable<IGenkidamaEventHandler<TEvent>> handlers,
        CancellationToken cancellationToken = default)
        where TEvent : IGenkidamaEvent;
}
