using Genkidama.Contracts;

namespace Genkidama.Events;

/// <summary>
/// Publishes events to handlers in sequence.
/// </summary>
public sealed class GenkidamaEventPublisher : IGenkidamaEventPublisher
{
    /// <inheritdoc />
    public async Task<StandardResult> PublishAsync<TEvent>(
        TEvent eventItem,
        IEnumerable<IGenkidamaEventHandler<TEvent>> handlers,
        CancellationToken cancellationToken = default)
        where TEvent : IGenkidamaEvent
    {
        foreach (var handler in handlers)
        {
            var result = await handler.HandleAsync(eventItem, cancellationToken);
            if (result.Succeeded is false)
            {
                return result;
            }
        }

        return StandardResult.Success();
    }
}
