namespace Genkidama.Events;

/// <summary>
/// Describes a notification produced from an event.
/// </summary>
/// <param name="Id">The notification identifier.</param>
/// <param name="EventName">The source event name.</param>
/// <param name="Channel">The notification channel.</param>
/// <param name="Message">The notification message.</param>
public sealed record StandardNotification(
    Guid Id,
    string EventName,
    string Channel,
    string Message)
{
    /// <summary>
    /// Creates a standard notification.
    /// </summary>
    public static StandardNotification Create(string eventName, string channel, string message)
        => new(Guid.NewGuid(), eventName, channel, message);
}
