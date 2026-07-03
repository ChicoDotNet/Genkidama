using Genkidama.Contracts;

namespace Genkidama.Events;

/// <summary>
/// Delivers standard notifications.
/// </summary>
public interface IGenkidamaNotificationChannel
{
    /// <summary>
    /// Sends one notification.
    /// </summary>
    /// <param name="notification">The notification to send.</param>
    /// <param name="cancellationToken">The cancellation token.</param>
    /// <returns>The delivery result.</returns>
    Task<StandardResult> SendAsync(
        StandardNotification notification,
        CancellationToken cancellationToken = default);
}
