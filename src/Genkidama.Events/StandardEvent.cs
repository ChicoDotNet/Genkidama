namespace Genkidama.Events;

/// <summary>
/// Describes a standard Genkidama event.
/// </summary>
public sealed record StandardEvent(
    Guid Id,
    string Name,
    DateTimeOffset OccurredUtc) : IGenkidamaEvent
{
    /// <summary>
    /// Creates an event using the current UTC timestamp.
    /// </summary>
    public static StandardEvent Create(string name)
        => new(Guid.NewGuid(), name, DateTimeOffset.UtcNow);
}
