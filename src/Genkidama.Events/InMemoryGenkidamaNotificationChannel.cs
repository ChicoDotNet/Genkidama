using Genkidama.Contracts;

namespace Genkidama.Events;

/// <summary>
/// Stores sent notifications in memory for tests and generated samples.
/// </summary>
public sealed class InMemoryGenkidamaNotificationChannel : IGenkidamaNotificationChannel
{
    private readonly List<StandardNotification> notifications = [];

    /// <summary>
    /// Gets the sent notifications.
    /// </summary>
    public IReadOnlyList<StandardNotification> Sent => notifications;

    /// <inheritdoc />
    public Task<StandardResult> SendAsync(
        StandardNotification notification,
        CancellationToken cancellationToken = default)
    {
        notifications.Add(notification);
        return Task.FromResult(StandardResult.Success());
    }
}
