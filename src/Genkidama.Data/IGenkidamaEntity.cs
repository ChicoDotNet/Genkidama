namespace Genkidama.Data;

/// <summary>
/// Represents an entity with a stable identity.
/// </summary>
/// <typeparam name="TKey">The identity type.</typeparam>
public interface IGenkidamaEntity<out TKey>
{
    /// <summary>
    /// Gets the entity identifier.
    /// </summary>
    TKey Id { get; }
}
