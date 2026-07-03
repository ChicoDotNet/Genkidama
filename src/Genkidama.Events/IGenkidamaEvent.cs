namespace Genkidama.Events;

/// <summary>
/// Represents an event that happened inside a Genkidama application.
/// </summary>
public interface IGenkidamaEvent
{
    /// <summary>
    /// Gets the event identifier.
    /// </summary>
    Guid Id { get; }

    /// <summary>
    /// Gets the event name.
    /// </summary>
    string Name { get; }

    /// <summary>
    /// Gets the UTC timestamp when the event happened.
    /// </summary>
    DateTimeOffset OccurredUtc { get; }
}
